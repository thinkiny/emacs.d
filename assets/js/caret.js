/** CaretEmacs – Emacs-like caret navigation for WebKit. */

/* ── Constants ────────────────────────────────────────────── */

const CURSOR_ID = "__caret-emacs-cursor";
const STYLE_ID = "__caret-emacs-style";
const WORD_CHAR_RE = /[\p{L}\p{N}\p{M}\p{Pc}]/u;
const cloneRect = (r) => ({ top: r.top, left: r.left, width: r.width, height: r.height });

const CURSOR_CSS = `
#${CURSOR_ID}{
  position:absolute;pointer-events:none;z-index:2147483647;
  background:transparent;display:none;box-sizing:border-box;
  border-radius:1px;
  box-shadow: 0 0 0 1px CanvasText;
}`.trim();

class CaretEmacs {

  /* ── Construction ──────────────────────────────────────────── */

  constructor(el = document, opts = {}) {
    this.el = el;
    this.markActive = false;
    this._debug = true;
    this._debugLog = [];
    this._onSelectionChange = this._updateCursor.bind(this);
    this.scrollContainer = opts.scrollContainer || null;
    this._scrollPx = opts.scrollPx || 200;
    this._scrollDownFraction = opts.scrollDownFraction || 1 / 3;
    this._scrollUpFraction = opts.scrollUpFraction || 2 / 3;
    this._cursorEl = null;
    this._scrollRafPending = false;
    this._lastScrollTop = 0;
    this._suppressScrollRelocate = false;

    // Performance caches
    this._visualOrderCache = null;       // { root, ordered, lines, frameId }
    this._visualOrderGen = 0;
    this._fontSizeCache = new WeakMap();
    this._textBoundsCache = new WeakMap();

    this._onKeyDown = (e) => {
      if (e.ctrlKey && e.key === 'g' && !e.shiftKey && !e.altKey && !e.metaKey)
        this.deactivateMark();
    };
    this._onScroll = () => this._onUserScroll();
    this._onResize = () => {
      requestAnimationFrame(() => { this._invalidateLayoutCaches(); this._updateCursor(); });
    };

    const init = () => {
      this._initCursor();
      document.addEventListener("selectionchange", this._onSelectionChange);
      document.addEventListener("keydown", this._onKeyDown);
      if (this.scrollContainer) {
        this._initPdfScroll();
      } else {
        this._ensureSelection();
        this._updateCursor();
        window.addEventListener('scroll', this._onScroll, { passive: true });
        window.addEventListener('resize', this._onResize, { passive: true });
      }
    };
    document.body ? init() : document.addEventListener("DOMContentLoaded", init, { once: true });
  }

  get _root() { return this.el === document ? document.body : this.el; }

  _invalidateLayoutCaches() {
    this._visualOrderGen++;
    this._fontSizeCache = new WeakMap();
    this._textBoundsCache = new WeakMap();
  }

  /* ── Debug ─────────────────────────────────────────────────── */

  dumpDebug() {
    return this._debugLog.map((entry) => JSON.stringify(entry)).join("\n");
  }

  _logDebug(event, data) {
    if (!this._debug) return;
    this._debugLog.push({ event, ...data });
  }

  /* ── Viewport & Scroll ─────────────────────────────────────── */

  get _scrollTop() {
    return this.scrollContainer
      ? this.scrollContainer.scrollTop : window.scrollY;
  }

  get _viewportHeight() {
    return this.scrollContainer
      ? this.scrollContainer.clientHeight : window.innerHeight;
  }

  get _scrollHeight() {
    return this.scrollContainer
      ? this.scrollContainer.scrollHeight
      : document.documentElement.scrollHeight;
  }

  _viewportRect() {
    if (this.scrollContainer)
      return this.scrollContainer.getBoundingClientRect();
    return { top: 0, left: 0, right: window.innerWidth, bottom: window.innerHeight };
  }

  _scrollBy(dy) {
    if (this.scrollContainer) this.scrollContainer.scrollTop += dy;
    else window.scrollBy(0, dy);
  }

  _scrollTo(y) {
    if (this.scrollContainer) this.scrollContainer.scrollTop = y;
    else window.scrollTo(0, y);
  }

  _scrollToSelectionLineBounded() {
    const sel = window.getSelection();
    if (!sel?.rangeCount) {
      return false;
    }

    const focusRect = this._selectionFocusRect(sel);
    if (!focusRect) {
      return false;
    }

    const top = focusRect.top;
    const bottom = focusRect.bottom ?? (focusRect.top + focusRect.height);
    const viewportRect = this._viewportRect();

    if (bottom > viewportRect.bottom) {
      if (Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight) return false;
      const delta = Math.min(this._scrollPx, Math.max(0, bottom - viewportRect.bottom));
      if (delta <= 0) return false;
      this._scrollBy(delta);
      return true;
    }
    if (top < viewportRect.top) {
      if (Math.floor(this._scrollTop) <= 0) return false;
      const delta = -Math.min(this._scrollPx, Math.max(0, viewportRect.top - top));
      if (delta >= 0) return false;
      this._scrollBy(delta);
      return true;
    }

    return false;
  }

  /** Scroll the viewport to keep the selection focus visible. Returns true if scrolled. */
  _scrollToSelection() {
    const sel = window.getSelection();
    if (!sel?.rangeCount) {
      return false;
    }

    const focusRect = this._selectionFocusRect(sel);
    if (!focusRect) {
      return false;
    }

    const top = focusRect.top;
    const bottom = focusRect.bottom ?? (focusRect.top + focusRect.height);
    const viewportRect = this._viewportRect();

    if (bottom > viewportRect.bottom) {
      if (Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight) return false;
      // Scroll to place cursor at 1/3 from bottom of viewport
      const viewportHeight = this._viewportHeight;
      const delta = bottom - (viewportRect.top + viewportHeight * this._scrollDownFraction);
      if (delta <= 0) return false;
      this._scrollBy(delta);
      return true;
    } else if (top < viewportRect.top) {
      if (Math.floor(this._scrollTop) <= 0) return false;
      // Scroll to place cursor at 2/3 from top of viewport
      const viewportHeight = this._viewportHeight;
      const delta = top - (viewportRect.top + viewportHeight * this._scrollUpFraction);
      if (delta >= 0) return false;
      this._scrollBy(delta);
      return true;
    }
    return false;
  }

  /** Page down/up: scroll viewport, then re-place caret using caretRangeFromPoint. */
  _scrollPage(direction) {
    const sel = this._ensureSelection();
    if (!sel?.rangeCount) return;
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    const caretX = rect.left;
    const caretY = rect.top;
    const delta = this._viewportHeight / 3;
    this._scrollBy(direction === "down" ? delta : -delta);
    const newY = direction === "down" ? caretY - delta : caretY + delta;
    const range = this._probeTextAt(caretX, newY);
    if (!range) return;
    this._applyRange(sel, range);
    this._updateCursor();
  }

  _initPdfScroll() {
    const placeCaret = () => { this._ensureSelection(); this._updateCursor(); };
    const poll = () => {
      const bus = window.PDFViewerApplication?.eventBus;
      if (!bus) { requestAnimationFrame(poll); return; }
      let placed = false;
      const ensureCaret = () => {
        if (placed) return;
        placed = true;
        const pageNum = window.PDFViewerApplication?.page;
        if (document.querySelector(`.page[data-page-number="${pageNum}"] .textLayer`)?.children.length) { placeCaret(); return; }
        const onRendered = (e) => {
          if (e.pageNumber !== pageNum) return;
          bus.off('textlayerrendered', onRendered);
          this._visualOrderGen++;
          placeCaret();
        };
        bus.on('textlayerrendered', onRendered);
      };
      this._onPdfScroll = () => {
        ensureCaret();
        this._onUserScroll();
      };
      this.scrollContainer.addEventListener('scroll', this._onPdfScroll, { passive: true });
      setTimeout(ensureCaret, 300);
    };
    poll();
  }

  _onUserScroll() {
    if (this._scrollRafPending) return;
    this._scrollRafPending = true;
    requestAnimationFrame(() => {
      this._scrollRafPending = false;
      this._visualOrderGen++;
      const scrollTop = this._scrollTop;
      const isForward = scrollTop > this._lastScrollTop;
      this._lastScrollTop = scrollTop;

      const sel = window.getSelection();
      if (!sel?.rangeCount) {
        this._ensureSelection();
        this._updateCursor();
        return;
      }

      const viewportRect = this._viewportRect();
      const caretRect = this._isContained(sel.focusNode)
        ? this._rangeRectAt(sel.focusNode, sel.focusOffset) : null;
      if (this._isRectInViewport(caretRect, viewportRect)) return;
      if (this._suppressScrollRelocate) {
        this._suppressScrollRelocate = false;
        return;
      }

      // Use caret's current X (still valid even if off-screen), or viewport center
      const cx = caretRect
        ? caretRect.left + caretRect.width / 2
        : (viewportRect.left + viewportRect.right) / 2;
      const cy = isForward ? viewportRect.top + 20 : viewportRect.bottom - 20;

      const resolved = this._probeTextAt(cx, cy);
      if (resolved) {
        this._setSelectionRange(sel, resolved);
        this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
      }
      this._updateCursor();
    });
  }

  /* ── Cursor Overlay ────────────────────────────────────────── */

  _initCursor() {
    if (!document.getElementById(STYLE_ID)) {
      const style = Object.assign(document.createElement("style"),
        { id: STYLE_ID, textContent: CURSOR_CSS });
      document.head.appendChild(style);
    }
    this._cursorEl = document.getElementById(CURSOR_ID)
      ?? (() => {
        const el = Object.assign(document.createElement("div"), { id: CURSOR_ID });
        document.documentElement.appendChild(el);
        return el;
      })();
  }

  _updateCursor() {
    const el = this._cursorEl;
    if (!el) return;
    const sel = window.getSelection();
    if (!sel?.rangeCount) {
      // Selection unexpectedly lost — restore from last rendered position
      const c = this._lastRenderedPos;
      if (c?.node && this._root.contains(c.node)) {
        sel.removeAllRanges();
        sel.addRange(this._collapsedRange(c.node, c.offset));
      }
      if (!sel?.rangeCount) { el.style.display = "none"; return; }
    }
    // Skip redundant redraw if position unchanged
    const lrp = this._lastRenderedPos;
    if (lrp && sel.focusNode === lrp.node && sel.focusOffset === lrp.offset
      && el.style.display === "block") return;

    if (!this._isContained(sel.focusNode)) { el.style.display = "none"; return; }
    const { node, offset } = this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    const rect = this._cursorRectAt(node, offset);
    if (!rect?.height) { el.style.display = "none"; return; }
    const cw = this._cursorWidth(rect);
    let cursorTop = rect.top + window.scrollY;
    let cursorLeft = rect.left + window.scrollX;
    let cursorHeight = rect.height;
    const parent = node.nodeType === Node.TEXT_NODE ? node.parentElement : node;
    if (parent) {
      let fontSize = this._fontSizeCache.get(parent);
      if (fontSize === undefined) {
        fontSize = parseFloat(getComputedStyle(parent).fontSize);
        this._fontSizeCache.set(parent, fontSize);
      }
      if (fontSize > 0 && cursorHeight > fontSize) {
        cursorTop += (cursorHeight - fontSize) / 2;
        cursorHeight = fontSize;
      }
    }
    el.style.cssText = `display:block;left:${cursorLeft}px;top:${cursorTop}px;width:${cw}px;height:${cursorHeight}px`;

    // Save current focus as last-rendered position
    this._lastRenderedPos = { node: sel.focusNode, offset: sel.focusOffset };
  }

  /** Get a client rect for cursor display at the given text position. */
  _cursorRectAt(node, offset) {
    const rect = this._rangeRectAt(node, offset) || this._rangeRectAt(node, offset - 1);
    if (rect) return rect;
    const cr = this._collapsedRange(node, offset);
    return cr.getClientRects()[0] || cr.getBoundingClientRect();
  }

  /** Compute cursor width: use the rect's width if it looks like a single character, else approximate. */
  _cursorWidth(rect) {
    const isCharSized = rect.width > 1 && rect.width <= rect.height * 1.2;
    return isCharSized ? rect.width : rect.height * 0.6;
  }

  /* ── DOM Helpers ───────────────────────────────────────────── */

  _isContained(node) {
    return node && this._root.contains(node);
  }

  _isPdfMode() {
    return Boolean(this.scrollContainer);
  }

  /** Create a collapsed range at the given node and offset. */
  _collapsedRange(node, offset) {
    const range = document.createRange();
    const max = node.nodeType === Node.TEXT_NODE ? node.length : node.childNodes.length;
    range.setStart(node, Math.min(Math.max(0, offset), max));
    range.collapse(true);
    return range;
  }

  /** Make a 1-char range at (node, offset) and return its rect, or null. */
  _rangeRectAt(node, offset) {
    if (node.nodeType !== Node.TEXT_NODE || !node.length) return null;
    const range = document.createRange();
    const clampedOff = Math.max(0, Math.min(offset, node.length - 1));
    range.setStart(node, clampedOff);
    range.setEnd(node, clampedOff + 1);
    const rect = range.getClientRects()[0] || range.getBoundingClientRect();
    return (rect?.height && rect?.width) ? rect : null;
  }

  /** True when two rects share the same visual line. */
  _isSameLine(rectA, rectB) {
    const tolerance = Math.max(rectA.height, rectB.height) / 2;
    return Math.abs((rectA.top + rectA.height / 2) - (rectB.top + rectB.height / 2)) <= tolerance;
  }

  /** Return a TreeWalker for text nodes, positioned at `node`. */
  _textWalker(node) {
    const tw = document.createTreeWalker(this._root, NodeFilter.SHOW_TEXT);
    tw.currentNode = node;
    return tw;
  }

  /** Walk from `node` in direction `fwd`, returning the first visible text node, or null. */
  _walkToVisible(node, fwd) {
    if (!this._isContained(node)) return null;
    const tw = this._textWalker(node);
    let textNode = fwd ? tw.nextNode() : tw.previousNode();
    while (textNode && !this._isNavigableTextNode(textNode)) {
      textNode = fwd ? tw.nextNode() : tw.previousNode();
    }
    return textNode;
  }

  /** True if moving from (node0,off0) to (node1,off1) goes the wrong way. */
  _movedWrongWay(node0, off0, node1, off1, fwd) {
    try {
      const r0 = document.createRange(); r0.setStart(node0, off0);
      const r1 = document.createRange(); r1.setStart(node1, off1);
      const cmp = r0.compareBoundaryPoints(Range.START_TO_START, r1);
      return (fwd && cmp > 0) || (!fwd && cmp < 0);
    } catch (e) { return false; }
  }

  _preservesWhitespace(node) {
    const el = node?.nodeType === Node.ELEMENT_NODE ? node : node?.parentElement;
    if (!el) return false;
    const ws = getComputedStyle(el).whiteSpace;
    return ws === 'pre' || ws === 'pre-wrap' || ws === 'break-spaces';
  }

  _isNavigableTextNode(textNode) {
    if (textNode?.nodeType !== Node.TEXT_NODE) return false;
    const text = textNode.textContent || "";
    if (!text.length) return false;
    return this._preservesWhitespace(textNode) || !!text.trim();
  }

  /** Return visible text bounds within a text node, excluding leading/trailing whitespace except preserved whitespace contexts. */
  _textVisibleBounds(textNode) {
    if (textNode.nodeType !== Node.TEXT_NODE) return { start: 0, end: 0, length: 0 };
    const cached = this._textBoundsCache.get(textNode);
    if (cached) return cached;
    const text = textNode.textContent || "";
    const length = text.length;
    if (this._preservesWhitespace(textNode)) return { start: 0, end: length, length };
    if (!text.trim()) return { start: 0, end: length, length };

    const start = text.search(/\S/u);
    const end = text.trimEnd().length || length;
    const result = { start: Math.max(0, start), end, length };
    this._textBoundsCache.set(textNode, result);
    return result;
  }

  /** Return start/end caret edge for visible content in a text node. */
  _textVisibleEdgeOffset(textNode, atStart) {
    const { start, end } = this._textVisibleBounds(textNode);
    return atStart ? start : end;
  }

  /** Clamp a caret offset to visible content bounds when possible. */
  _normalizeTextOffset(node, offset) {
    if (node.nodeType !== Node.TEXT_NODE) return offset;
    const { start, end, length } = this._textVisibleBounds(node);
    const clamped = Math.min(Math.max(0, offset), length);
    if (clamped < start) return start;
    if (clamped > end) return end;
    return clamped;
  }

  /** Resolve element/whitespace focus to a visible text position. */
  _resolveCursorPosition(node, offset, preferFwd = false) {
    if (node.nodeType === Node.ELEMENT_NODE) {
      const child = offset < node.childNodes.length
        ? node.childNodes[offset]
        : node.lastChild;

      // Quick check: direct child or its firstChild (for preferFwd)
      if (preferFwd && child) {
        const text = child.nodeType === Node.TEXT_NODE ? child
          : child.firstChild?.nodeType === Node.TEXT_NODE ? child.firstChild : null;
        if (this._isNavigableTextNode(text))
          return { node: text, offset: this._textVisibleEdgeOffset(text, true) };
      }

      const start = child || node;
      const t = this._walkToVisible(start, true) || this._walkToVisible(start, false);
      if (t) return { node: t, offset: this._textVisibleEdgeOffset(t, true) };
    }
    if (node.nodeType === Node.TEXT_NODE && !(node.textContent || "").trim()) {
      const prev = this._walkToVisible(node, false);
      if (prev) return { node: prev, offset: this._textVisibleEdgeOffset(prev, false) };
      const next = this._walkToVisible(node, true);
      if (next) return { node: next, offset: this._textVisibleEdgeOffset(next, true) };
    }
    return { node, offset: this._normalizeTextOffset(node, offset) };
  }

  /** Resolve a range from caretRangeFromPoint to a visible text node, or null. */
  _rangeToText(range) {
    const { node, offset } = this._resolveCursorPosition(
      range.startContainer, range.startOffset, true);
    if (node.nodeType !== Node.TEXT_NODE || !this._isNavigableTextNode(node)) return null;
    return this._collapsedRange(node, offset);
  }

  /** Get a rect with real dimensions at a range's start position. */
  _charRect(range) {
    const rect = this._rangeRectAt(range.startContainer, range.startOffset);
    if (rect) return rect;
    const fallbackRect = range.getBoundingClientRect();
    return fallbackRect?.height ? fallbackRect : null;
  }

  /** Rect used for line movement; keeps caret-before-newline on the current line. */
  _lineMoveRect(node, offset) {
    if (node?.nodeType === Node.TEXT_NODE) {
      const text = node.textContent || "";
      const len = text.length;
      const off = Math.max(0, Math.min(offset, len));
      if (off > 0 && off < len && text[off] === '\n') {
        const prevRect = this._rangeRectAt(node, off - 1);
        if (prevRect) return prevRect;
      }
      const curRect = this._rangeRectAt(node, off);
      if (curRect) return curRect;
      if (off > 0) {
        const prevRect = this._rangeRectAt(node, off - 1);
        if (prevRect) return prevRect;
      }
      const fallback = this._collapsedRange(node, off).getBoundingClientRect();
      return fallback?.height ? fallback : null;
    }
    return this._charRect(this._collapsedRange(node, offset));
  }

  /** Resolve the current selection focus to a visible rect, or null. */
  _selectionFocusRect(sel) {
    if (!sel?.focusNode) return null;
    const focusRect = this._rangeRectAt(sel.focusNode, sel.focusOffset)
      || this._collapsedRange(sel.focusNode, sel.focusOffset).getBoundingClientRect();
    return focusRect?.height ? focusRect : null;
  }

  /** True when rect overlaps the viewport vertically. */
  _isRectInViewport(rect, viewportRect = this._viewportRect()) {
    if (!rect?.height) return false;
    const rectBottom = rect.bottom ?? (rect.top + rect.height);
    return rectBottom >= viewportRect.top && rect.top <= viewportRect.bottom;
  }

  /** Replace selection with a single range. */
  _setSelectionRange(sel, range) {
    sel.removeAllRanges();
    sel.addRange(range);
  }

  /** Probe for a text range at screen coordinates; returns a collapsed Range or null. */
  _probeTextAt(cx, cy) {
    const range = document.caretRangeFromPoint(cx, cy);
    if (!range || !this._isContained(range.startContainer)) return null;
    if (this.scrollContainer) {
      const parentEl = range.startContainer.nodeType === Node.TEXT_NODE
        ? range.startContainer.parentElement : range.startContainer;
      if (!parentEl?.closest('.textLayer')) return null;
    }
    return this._rangeToText(range);
  }

  /* ── Selection Management ──────────────────────────────────── */

  _ensureSelection(skipRelocate = false) {
    const sel = window.getSelection();
    if (sel?.rangeCount > 0) {
      if (!skipRelocate) this._relocateIfOffscreen(sel);
      return sel;
    }
    const c = this._savedFocus;
    if (c?.node && this._root.contains(c.node)) {
      const r = this._collapsedRange(c.node, c.offset);
      sel.removeAllRanges();
      sel.addRange(r);
    } else {
      const range = document.createRange();
      range.selectNodeContents(this._root);
      range.collapse(true);
      sel.removeAllRanges();
      sel.addRange(range);
    }
    if (!skipRelocate) this._relocateIfOffscreen(sel);
    return sel;
  }

  /** Re-place the caret at visible content when it is outside the viewport. */
  _relocateIfOffscreen(sel) {
    const focusRect = this._selectionFocusRect(sel);
    const viewportRect = this._viewportRect();
    if (this._isRectInViewport(focusRect, viewportRect)) return;
    const resolved = this._probeTextAt((viewportRect.left + viewportRect.right) / 2, viewportRect.top + 1);
    if (resolved) {
      this._setSelectionRange(sel, resolved);
    }
  }

  _restoreCaretIfLost() {
    const sel = window.getSelection();
    if (sel?.rangeCount) return;
    const c = this._savedFocus;
    if (!c?.node || !this._root.contains(c.node)) return;
    const r = this._collapsedRange(c.node, c.offset);
    sel.removeAllRanges();
    sel.addRange(r);
    this._updateCursor();
  }

  _setMark(active) {
    this.markActive = active;
    if (!active) {
      const sel = window.getSelection();
      if (sel?.rangeCount) {
        const focus = sel.focusNode;
        const focusOff = sel.focusOffset;
        sel.collapse(focus, focusOff);
        this._savedFocus = { node: focus, offset: focusOff };
        setTimeout(() => this._restoreCaretIfLost(), 0);
      }
    }
    this._updateCursor();
  }

  _applyRange(sel, range) {
    if (this.markActive) {
      sel.setBaseAndExtent(sel.anchorNode, sel.anchorOffset,
        range.startContainer, range.startOffset);
    } else {
      sel.removeAllRanges();
      sel.addRange(range);
    }
  }

  /* ── Visual Ordering ───────────────────────────────────────── */

  _pageScopeRoot(node) {
    const el = node.nodeType === Node.ELEMENT_NODE ? node : node.parentElement;
    return el?.closest('.page[data-page-number]')
      || el?.closest('.textLayer')
      || this._currentPage()
      || this._root;
  }

  _currentPage() {
    if (!this._isPdfMode()) return null;
    const sel = window.getSelection();
    if (sel?.focusNode) {
      const el = sel.focusNode.nodeType === Node.ELEMENT_NODE
        ? sel.focusNode : sel.focusNode.parentElement;
      const page = el?.closest('.page[data-page-number]');
      if (page) return page;
    }
    // Fallback: element at viewport center
    const vp = this._viewportRect();
    const cx = (vp.left + vp.right) / 2;
    const cy = (vp.top + vp.bottom) / 2;
    return document.elementFromPoint(cx, cy)?.closest('.page[data-page-number]') || null;
  }

  _visuallyAdjacentPage(currentPage, fwd) {
    const pages = Array.from(this._root.querySelectorAll('.page[data-page-number]'));
    if (pages.length < 2) return null;
    pages.sort((a, b) => {
      const ra = a.getBoundingClientRect(), rb = b.getBoundingClientRect();
      const dy = ra.top - rb.top;
      return Math.abs(dy) > 5 ? dy : ra.left - rb.left;
    });
    const idx = pages.indexOf(currentPage);
    if (idx === -1) return null;
    const adjIdx = fwd ? idx + 1 : idx - 1;
    return (adjIdx >= 0 && adjIdx < pages.length) ? pages[adjIdx] : null;
  }

  _visuallyOrderedTextNodes(root) {
    // Return from cache if valid this frame
    const c = this._visualOrderCache;
    if (c && c.root === root && c.frameId === this._visualOrderGen) return c.ordered;

    const tw = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = [];
    let domIndex = 0;
    while (tw.nextNode()) {
      const textNode = tw.currentNode;
      if (!this._isNavigableTextNode(textNode)) continue;
      const segments = this._splitNodeIntoLineSegments(textNode, domIndex++, root);
      for (const seg of segments) nodes.push(seg);
    }
    if (!nodes.length) {
      this._visualOrderCache = { root, ordered: [], lines: [], frameId: this._visualOrderGen };
      return [];
    }

    nodes.sort((a, b) => {
      const aMid = a.rect.top + a.rect.height / 2;
      const bMid = b.rect.top + b.rect.height / 2;
      return (aMid - bMid) ||
        (a.rect.left - b.rect.left) ||
        (a.domIndex - b.domIndex);
    });

    const lines = this._groupIntoLines(nodes);
    const ordered = [];
    for (const line of lines) {
      line.sort((a, b) =>
        (a.rect.left - b.rect.left) ||
        (a.domIndex - b.domIndex));
      for (const entry of line) {
        ordered.push({
          node: entry.node, rect: entry.rect,
          startOffset: entry.startOffset, endOffset: entry.endOffset
        });
      }
    }

    this._visualOrderCache = { root, ordered, lines, frameId: this._visualOrderGen };
    return ordered;
  }

  /** Return cached { ordered, lines } for a scope root. */
  _visuallyOrderedWithLines(root) {
    this._visuallyOrderedTextNodes(root); // ensures cache is populated
    const c = this._visualOrderCache;
    return (c && c.root === root) ? { ordered: c.ordered, lines: c.lines } : { ordered: [], lines: [] };
  }

  /** Split a text node into per-visual-line segments.
   *  Returns [{ node, rect, startOffset, endOffset, domIndex }]. */
  _splitNodeIntoLineSegments(textNode, domIndex, scopeRoot = null) {
    const groupRoot = this._isPdfMode() ? null : this._lineGroupingRoot(textNode, scopeRoot);
    const range = document.createRange();
    range.selectNodeContents(textNode);
    const rects = Array.from(range.getClientRects()).filter(r => r.height && r.width);
    if (!rects.length) return [];
    // Fast path: single visual line (common in PDF spans and short HTML text)
    // Group fragment rects into visual lines by Y-band
    const lineRects = rects.length === 1 ? rects : this._groupFragmentRects(rects);
    if (lineRects.length <= 1) {
      return [{
        node: textNode, rect: cloneRect(lineRects[0]),
        startOffset: 0, endOffset: textNode.length, domIndex, groupRoot
      }];
    }
    // Binary-search break offsets between consecutive visual lines
    const segments = [];
    let segStart = 0;
    for (let li = 0; li < lineRects.length; li++) {
      const lr = lineRects[li];
      const lrMid = lr.top + lr.height / 2;
      if (li < lineRects.length - 1) {
        // Find the first offset that belongs to the next line
        const nextLr = lineRects[li + 1];
        const nextMid = nextLr.top + nextLr.height / 2;
        let lo = segStart, hi = textNode.length;
        while (lo < hi) {
          const mid = (lo + hi) >>> 1;
          const cr = this._rangeRectAt(textNode, mid);
          if (!cr) { lo = mid + 1; continue; }
          const charMid = cr.top + cr.height / 2;
          // Does this char belong to the next line or beyond?
          if (Math.abs(charMid - nextMid) < Math.abs(charMid - lrMid)) {
            hi = mid;
          } else {
            lo = mid + 1;
          }
        }
        if (lo > segStart) {
          segments.push({
            node: textNode, rect: cloneRect(lr),
            startOffset: segStart, endOffset: lo, domIndex, groupRoot
          });
        }
        segStart = lo;
      } else {
        // Last line: rest of the text
        if (textNode.length > segStart) {
          segments.push({
            node: textNode, rect: cloneRect(lr),
            startOffset: segStart, endOffset: textNode.length, domIndex, groupRoot
          });
        }
      }
    }
    return segments;
  }

  /** Nearest non-inline ancestor used as an HTML line-group boundary. */
  _lineGroupingRoot(textNode, scopeRoot = null) {
    const scopeEl = scopeRoot?.nodeType === Node.ELEMENT_NODE ? scopeRoot : this._root;
    let el = textNode?.parentElement || null;

    while (el && el !== scopeEl) {
      const style = getComputedStyle(el);
      const display = style.display;
      const position = style.position;

      // Out-of-flow elements don't define line-group boundaries.
      if (position === 'absolute' || position === 'fixed') {
        el = el.parentElement;
        continue;
      }

      const parent = el.parentElement;
      const parentStyle = parent ? getComputedStyle(parent) : null;
      const parentDisplay = parentStyle?.display || '';
      const parentFlow = parentStyle?.flexDirection || '';
      const isInlineSelf = display === 'inline' || display === 'contents';
      const isRowItem = parent && (
        display === 'list-item'
        || ((parentDisplay === 'flex' || parentDisplay === 'inline-flex') && !parentFlow.startsWith('column'))
        || parentDisplay === 'grid'
        || parentDisplay === 'inline-grid'
        || (display.startsWith('inline-') && parentDisplay !== 'inline' && parentDisplay !== 'contents')
      );

      if (isRowItem && parent && parent !== scopeEl) {
        return parent;
      }
      if (!isInlineSelf) {
        return el;
      }
      el = parent;
    }

    return scopeEl;
  }

  /** Group an array of DOMRects by Y-band, returning one representative rect per visual line. */
  _groupFragmentRects(rects) {
    const lineRects = [rects[0]];
    for (let i = 1; i < rects.length; i++) {
      if (!this._isSameLine(rects[i], lineRects[lineRects.length - 1])) {
        lineRects.push(rects[i]);
      }
    }
    return lineRects;
  }

  _groupIntoLines(orderedNodes) {
    if (!orderedNodes.length) return [];
    const lines = [];
    let currentLine = [orderedNodes[0]];
    for (let i = 1; i < orderedNodes.length; i++) {
      const entry = orderedNodes[i];
      const firstRect = currentLine[0].rect;
      const sameVisualLine = this._isSameLine(entry.rect, firstRect);
      const sameGroup = this._isPdfMode() || entry.groupRoot === currentLine[0].groupRoot
        || currentLine[0].groupRoot?.contains(entry.groupRoot)
        || entry.groupRoot?.contains(currentLine[0].groupRoot);
      // Detect column break: horizontal gap exceeding entry height means
      // the entry belongs to a different column on the same visual line.
      let sameColumn = true;
      if (sameVisualLine && sameGroup && currentLine.length > 0) {
        const last = currentLine[currentLine.length - 1];
        const hGap = entry.rect.left - (last.rect.left + last.rect.width);
        sameColumn = hGap <= Math.max(entry.rect.height, last.rect.height);
      }
      if (sameVisualLine && sameColumn && sameGroup) {
        currentLine.push(entry);
      } else {
        lines.push(currentLine);
        currentLine = [entry];
      }
    }
    lines.push(currentLine);
    return lines;
  }

  _findCaretLine(lines, caretRect) {
    const caretMidY = caretRect.top + caretRect.height / 2;
    let bestIdx = -1, bestDist = Infinity;
    for (let i = 0; i < lines.length; i++) {
      const firstRect = lines[i][0].rect;
      const lineMidY = firstRect.top + firstRect.height / 2;
      const dist = Math.abs(caretMidY - lineMidY);
      if (dist < bestDist) { bestDist = dist; bestIdx = i; }
    }
    if (bestIdx < 0 || !this._isSameLine(caretRect, lines[bestIdx][0].rect)) return -1;
    // Among lines at the same Y, prefer the one containing the caret's X.
    // Reuse _isSameLine for Y-proximity (no new thresholds).
    const caretX = caretRect.left + caretRect.width / 2;
    const bestRect = lines[bestIdx][0].rect;
    for (let i = 0; i < lines.length; i++) {
      const firstRect = lines[i][0].rect;
      if (!this._isSameLine(firstRect, bestRect)) continue;
      const b = this._lineBounds(lines[i]);
      if (b && caretX >= b.left && caretX <= b.right) return i;
    }
    return bestIdx;
  }

  _pickPositionOnLine(line, goalX) {
    let bestRange = null, bestDist = Infinity;
    for (const entry of line) {
      const start = entry.startOffset ?? 0;
      const end = entry.endOffset ?? entry.node.length;
      for (let off = start; off < end; off++) {
        const r = document.createRange();
        r.setStart(entry.node, off); r.setEnd(entry.node, off + 1);
        const cr = r.getBoundingClientRect();
        if (!cr.width || !cr.height) continue;
        const d = Math.abs(cr.left + cr.width / 2 - goalX);
        if (d < bestDist) {
          bestDist = d;
          bestRange = this._collapsedRange(entry.node, off);
        }
      }
    }
    return bestRange;
  }

  /** Find the visually next/previous text node using visual ordering. */
  _visuallyAdjacentTextNode(fwd) {
    const sel = window.getSelection();
    if (!sel?.focusNode) return null;
    const focus = sel.focusNode;
    const scopeRoot = this._pageScopeRoot(focus);
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    const curIdx = ordered.findIndex(e => e.node === focus);
    if (curIdx === -1) return null;
    const targetIdx = fwd ? curIdx + 1 : curIdx - 1;
    if (targetIdx < 0 || targetIdx >= ordered.length) return null;
    return ordered[targetIdx].node;
  }

  _lineBounds(line) {
    if (!line?.length) return null;
    const first = line[0];
    const last = line[line.length - 1];
    let top = first.rect.top;
    let bottom = first.rect.top + first.rect.height;
    let height = first.rect.height;
    for (const entry of line) {
      top = Math.min(top, entry.rect.top);
      bottom = Math.max(bottom, entry.rect.top + entry.rect.height);
      height = Math.max(height, entry.rect.height);
    }
    return {
      first,
      last,
      left: first.rect.left,
      right: last.rect.left + last.rect.width,
      top,
      bottom,
      height: Math.max(height, bottom - top)
    };
  }

  /* ── Character & Word Movement ─────────────────────────────── */

  /** Unicode-aware word character classifier (handles CJK and other scripts). */
  _isWordChar(ch) {
    return Boolean(ch) && WORD_CHAR_RE.test(ch);
  }

  /** Visual character movement for PDF text layers (bypass DOM order). */
  _moveCharVisual(sel, fwd) {
    const focus = sel.focusNode;
    const focusOff = sel.focusOffset;
    if (focus.nodeType !== Node.TEXT_NODE) return false;

    if (fwd) {
      if (focusOff < focus.length) {
        sel.collapse(focus, focusOff + 1);
        return true;
      }
      const next = this._visuallyAdjacentTextNode(true);
      if (!next) return false;
      sel.collapse(next, 0);
      return true;
    } else {
      if (focusOff > 0) {
        sel.collapse(focus, focusOff - 1);
        return true;
      }
      const prev = this._visuallyAdjacentTextNode(false);
      if (!prev) return false;
      sel.collapse(prev, prev.length);
      return true;
    }
  }

  /** Visual word movement for PDF text layers (bypass DOM order). */
  _moveWordVisual(sel, fwd) {
    const focus = sel.focusNode;
    const focusOff = sel.focusOffset;
    if (focus.nodeType !== Node.TEXT_NODE) return false;

    const scopeRoot = this._pageScopeRoot(focus);
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return false;

    const curIdx = ordered.findIndex(e => e.node === focus);
    if (curIdx === -1) return false;

    if (fwd) {
      let node = focus, off = focusOff, idx = curIdx;
      // Skip past current word
      while (off < node.textContent.length && this._isWordChar(node.textContent[off])) off++;
      // Skip non-word chars, crossing node boundaries as needed
      while (true) {
        while (off < node.textContent.length && !this._isWordChar(node.textContent[off])) off++;
        if (off < node.textContent.length) {
          sel.collapse(node, off);
          return true;
        }
        idx++;
        if (idx >= ordered.length) return false;
        node = ordered[idx].node;
        off = 0;
      }
    } else {
      let node = focus, off = focusOff, idx = curIdx;
      // Step back one position
      if (off > 0) {
        off--;
      } else {
        idx--;
        if (idx < 0) return false;
        node = ordered[idx].node;
        off = node.textContent.length - 1;
      }
      // Skip non-word chars backward, crossing node boundaries
      while (true) {
        while (off >= 0 && !this._isWordChar(node.textContent[off])) off--;
        if (off >= 0) break;
        idx--;
        if (idx < 0) return false;
        node = ordered[idx].node;
        off = node.textContent.length - 1;
      }
      // Skip word chars backward to find word start
      while (off > 0 && this._isWordChar(node.textContent[off - 1])) off--;
      sel.collapse(node, off);
      return true;
    }
  }

  _wordStartOffsetInText(text, offset, fwd) {
    let idx = fwd
      ? Math.max(0, Math.min(offset, text.length))
      : Math.max(0, Math.min(offset, text.length)) - 1;
    while (idx >= 0 && idx < text.length && !this._isWordChar(text[idx])) {
      idx += fwd ? 1 : -1;
    }
    if (idx < 0 || idx >= text.length) return -1;
    while (idx > 0 && this._isWordChar(text[idx - 1])) idx--;
    return idx;
  }

  _findWordStart(node, offset, fwd) {
    let curNode = node;
    let curOff = offset;
    while (curNode) {
      const text = curNode.nodeType === Node.TEXT_NODE ? (curNode.textContent || "") : "";
      const idx = this._wordStartOffsetInText(text, curOff, fwd);
      if (idx >= 0) return { node: curNode, offset: idx };
      curNode = this._walkToVisible(curNode, fwd);
      curOff = fwd ? 0 : (curNode ? curNode.textContent.length : 0);
    }
    return null;
  }

  /** Normalize DOM word movement to the beginning of a word (HTML/EPUB path). */
  _snapToWordStart(sel, fwd) {
    const { node, offset } = this._resolveCursorPosition(sel.focusNode, sel.focusOffset, fwd);
    if (node.nodeType !== Node.TEXT_NODE) return false;
    const target = this._findWordStart(node, offset, fwd);
    if (!target?.node) return false;
    const normalizedOff = this._normalizeTextOffset(target.node, target.offset);
    if (target.node === sel.focusNode && normalizedOff === sel.focusOffset) return false;
    sel.collapse(target.node, normalizedOff);
    return true;
  }

  /** Snap the selection focus onto a visible text node. */
  _snapToText(sel, fwd) {
    const focus = sel.focusNode;
    if (focus.nodeType === Node.TEXT_NODE && this._isNavigableTextNode(focus)) return;

    const pastEnd = focus.nodeType === Node.ELEMENT_NODE &&
      fwd && sel.focusOffset >= focus.childNodes.length;
    const lookFwdFirst = pastEnd ? false
      : focus.nodeType === Node.ELEMENT_NODE ? fwd : !fwd;

    const textNode = this._walkToVisible(focus, lookFwdFirst)
      || this._walkToVisible(focus, !lookFwdFirst);
    if (textNode) {
      sel.collapse(textNode, this._textVisibleEdgeOffset(textNode, !fwd));
    }
  }

  /* ── Line Movement ─────────────────────────────────────────── */

  _lineTargetIndex(currentLineIndex, isForward, lines) {
    const currentBounds = this._lineBounds(lines[currentLineIndex]);
    const curRect = lines[currentLineIndex][0].rect;
    const start = isForward ? currentLineIndex + 1 : currentLineIndex - 1;
    const end = isForward ? lines.length : -1;
    for (let i = start; isForward ? i < end : i > end; i += isForward ? 1 : -1) {
      const targetRect = lines[i][0].rect;
      if (this._isSameLine(targetRect, curRect)) continue;
      const targetBounds = this._lineBounds(lines[i]);
      if (currentBounds && targetBounds
        && (currentBounds.right < targetBounds.left
          || targetBounds.right < currentBounds.left)) continue;
      return i;
    }
    return -1;
  }

  _lineVerticalGapPx(currentRect, targetRect, isForward) {
    return isForward
      ? targetRect.top - (currentRect.top + currentRect.height)
      : currentRect.top - (targetRect.top + targetRect.height);
  }

  _isLineWithinViewportTopThreshold(line, thresholdPx = this._scrollPx) {
    if (!line?.length) return false;
    const viewportRect = this._viewportRect();
    const lineTop = line[0].rect.top;
    return lineTop >= viewportRect.top && (lineTop - viewportRect.top) <= thresholdPx;
  }

  /** Move by logical line/column within a single preserved-whitespace text node. */
  _moveWithinPreservedTextNode(node, offset, fwd) {
    if (this._isPdfMode() || node?.nodeType !== Node.TEXT_NODE) return null;
    if (!this._preservesWhitespace(node)) return null;
    const text = node.textContent || "";
    if (!text.includes('\n')) return null;

    const len = text.length;
    const off = Math.max(0, Math.min(offset, len));
    const currentStart = text.lastIndexOf('\n', Math.max(0, off - 1)) + 1;
    let currentEnd = text.indexOf('\n', currentStart);
    if (currentEnd < 0) currentEnd = len;
    const clampedOff = Math.max(currentStart, Math.min(off, currentEnd));
    const column = clampedOff - currentStart;

    let targetStart = -1;
    let targetEnd = -1;
    if (fwd) {
      if (currentEnd >= len) return null;
      targetStart = currentEnd + 1;
      targetEnd = text.indexOf('\n', targetStart);
      if (targetEnd < 0) targetEnd = len;
    } else {
      if (currentStart <= 0) return null;
      targetEnd = currentStart - 1;
      const prevBreak = text.lastIndexOf('\n', Math.max(0, targetEnd - 1));
      targetStart = prevBreak + 1;
    }

    if (targetStart < 0 || targetEnd < targetStart) return null;
    // Skip empty or whitespace-only target segments — let DOM-based visual
    // line navigation handle the transition to the next visible text line.
    if (!text.substring(targetStart, targetEnd).trim()) return null;
    const targetOff = Math.min(targetStart + column, targetEnd);
    return this._collapsedRange(node, targetOff);
  }

  /** DOM-based visual line movement. Returns { range, scrolled }. */
  _moveLine(fwd) {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return { range: null, scrolled: false };

    const { node: lineNode, offset: lineOffset } =
      this._resolveCursorPosition(sel.focusNode, sel.focusOffset, true);
    const startRange = this._collapsedRange(lineNode, lineOffset);
    const caretRect = this._lineMoveRect(lineNode, lineOffset) || this._charRect(startRange);
    if (!caretRect?.height) return { range: null, scrolled: false };
    const goalX = caretRect.left + caretRect.width / 2;

    const preservedTextRange = this._moveWithinPreservedTextNode(lineNode, lineOffset, fwd);
    if (preservedTextRange) {
      return { range: preservedTextRange, scrolled: false };
    }

    // Phase 1: move within current scope (PDF page or full HTML body)
    const currentPage = this._currentPage();
    const scopeRoot = currentPage || this._root;
    const { ordered, lines } = this._visuallyOrderedWithLines(scopeRoot);
    if (!ordered.length) return { range: null, scrolled: false };
    if (!lines.length) return { range: null, scrolled: false };

    let currentLineIndex = this._findCaretLine(lines, caretRect);
    if (currentLineIndex < 0) {
      // Fallback: caret rect doesn't match any visual line — use closest by Y
      const midY = caretRect.top + caretRect.height / 2;
      let best = -1, bestD = Infinity;
      for (let i = 0; i < lines.length; i++) {
        const d = Math.abs(lines[i][0].rect.top + lines[i][0].rect.height / 2 - midY);
        if (d < bestD) { bestD = d; best = i; }
      }
      currentLineIndex = best;
    }
    if (currentLineIndex >= 0) {
      const targetLineIndex = this._lineTargetIndex(currentLineIndex, fwd, lines);
      if (targetLineIndex >= 0) {
        // Merge entries from adjacent line groups at the same visual Y
        const targetRect = lines[targetLineIndex][0].rect;
        const mergedLine = [...lines[targetLineIndex]];
        for (let j = targetLineIndex - 1; j >= 0; j--) {
          if (!this._isSameLine(lines[j][0].rect, targetRect)) break;
          mergedLine.unshift(...lines[j]);
        }
        for (let j = targetLineIndex + 1; j < lines.length; j++) {
          if (!this._isSameLine(lines[j][0].rect, targetRect)) break;
          mergedLine.push(...lines[j]);
        }
        return { range: this._pickPositionOnLine(mergedLine, goalX), scrolled: false };
      }

      // No target line in scope (HTML only) — scroll past non-text content
      if (!currentPage) {
        return this._scrollAndProbe(fwd, goalX, caretRect);
      }
    }

    // Phase 2 (PDF only): cross page boundary
    return { range: this._moveLineCrossPage(currentPage, fwd, goalX), scrolled: false };
  }

  /**
   * Scroll this._scrollPx toward non-text content, then probe for a text line
   * that has entered the near viewport. Returns a Range or null.
   */
  _scrollAndProbe(fwd, goalX, caretRect) {
    const canScroll = fwd
      ? Math.ceil(this._scrollTop + this._viewportHeight) < this._scrollHeight
      : Math.floor(this._scrollTop) > 0;
    if (!canScroll) return { range: null, scrolled: false };

    this._scrollBy(fwd ? this._scrollPx : -this._scrollPx);
    this._invalidateLayoutCaches();

    const scopeRoot = this._currentPage() || this._root;
    const { ordered, lines } = this._visuallyOrderedWithLines(scopeRoot);
    if (!ordered.length) return { range: null, scrolled: true };
    if (!lines.length) return { range: null, scrolled: true };

    // After scroll the caret shifted ±this._scrollPx in viewport coords;
    // find the closest in-viewport text line on the correct side.
    const viewportRect = this._viewportRect();
    const caretEdge = fwd
      ? caretRect.bottom - this._scrollPx
      : caretRect.top + this._scrollPx;
    const scan = fwd ? lines : [...lines].reverse();
    for (const line of scan) {
      const lineMidY = line[0].rect.top + line[0].rect.height / 2;
      if (lineMidY < viewportRect.top || lineMidY > viewportRect.bottom) continue;
      if (fwd ? lineMidY <= caretEdge : lineMidY >= caretEdge) continue;
      if (fwd && !this._isLineWithinViewportTopThreshold(line)) continue;
      return { range: this._pickPositionOnLine(line, goalX), scrolled: true };
    }
    return { range: null, scrolled: true };
  }

  /** Cross PDF page boundary for line movement. Returns a collapsed Range or null. */
  _moveLineCrossPage(page, fwd, goalX) {
    if (!page) return null;
    let adjacentPage = page;
    let skipped = 0;
    while ((adjacentPage = this._visuallyAdjacentPage(adjacentPage, fwd)) && skipped < 5) {
      const { ordered: adjacentOrdered, lines: adjacentLines } = this._visuallyOrderedWithLines(adjacentPage);
      if (!adjacentOrdered.length) { skipped++; continue; }
      // Found text — scroll into view if needed, then pick line
      const adjacentRect = adjacentPage.getBoundingClientRect();
      const viewportRect = this._viewportRect();
      if (adjacentRect.bottom < viewportRect.top || adjacentRect.top > viewportRect.bottom) {
        adjacentPage.scrollIntoView({ block: fwd ? 'start' : 'end' });
      }
      if (!adjacentLines.length) { skipped++; continue; }
      const targetLine = fwd ? adjacentLines[0] : adjacentLines[adjacentLines.length - 1];
      return this._pickPositionOnLine(targetLine, goalX);
    }
    // Scroll first adjacent page into view to trigger text layer loading
    const firstAdjacentPage = this._visuallyAdjacentPage(page, fwd);
    if (firstAdjacentPage) {
      firstAdjacentPage.scrollIntoView({ block: fwd ? 'start' : 'end' });
    }
    return null;
  }

  /* ── Movement Dispatcher ───────────────────────────────────── */

  _moveCaret(direction, granularity) {
    const sel = this._ensureSelection(true);
    if (!sel) { return false; }

    this._hitBoundary = false;
    if (this._isAtVisibleBoundary(direction)) {
      this._hitBoundary = true;
      return false;
    }

    // Pre-snap: if on whitespace node, snap to visible text first
    const { node: snapNode, offset: snapOff } =
      this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    if (snapNode !== sel.focusNode || snapOff !== sel.focusOffset) {
      sel.collapse(snapNode, snapOff);
    }

    if (!this.markActive && !sel.isCollapsed) {
      sel.collapse(sel.focusNode, sel.focusOffset);
    }

    const fwd = direction === "forward";
    const startNode = sel.focusNode, startOff = sel.focusOffset;
    const markAnchorNode = this.markActive ? sel.anchorNode : null;
    const markAnchorOff = this.markActive ? sel.anchorOffset : null;
    if (this.markActive) { sel.collapse(sel.focusNode, sel.focusOffset); }

    // PDF text layers require visual ordering because DOM order can differ from reading order.
    if (this._isPdfMode() && (granularity === "character" || granularity === "word")) {
      const moved = granularity === "character"
        ? this._moveCharVisual(sel, fwd)
        : this._moveWordVisual(sel, fwd);
      return this._finishMove(sel, moved, markAnchorNode, markAnchorOff);
    }

    // Line: unified visual line movement for both PDF and HTML.
    if (granularity === "line") {
      const preRect = this._rangeRectAt(startNode, startOff)
        || this._collapsedRange(startNode, startOff).getBoundingClientRect();
      const { range: lineRange } = this._moveLine(fwd);

      if (!lineRange) {
        const atVisibleBoundary = this._isAtVisibleBoundary(direction);
        const atViewportEdge = fwd ? this.isAtBottom() : this.isAtTop();
        this._hitBoundary = atVisibleBoundary || atViewportEdge;
        return false;
      }

      // Target off-screen (e.g. past an image): scroll _scrollPx without moving caret
      const targetRect = lineRange.getBoundingClientRect();
      if (targetRect.height) {
        const vpNow = this._viewportRect();
        const offscreen = fwd
          ? targetRect.top >= vpNow.bottom
          : targetRect.bottom <= vpNow.top;
        if (offscreen) {
          this._suppressScrollRelocate = true;
          this._scrollBy(fwd ? this._scrollPx : -this._scrollPx);
          this._invalidateLayoutCaches();
          this._updateCursor();
          return true;
        }
      }

      sel.removeAllRanges();
      sel.addRange(lineRange);

      const vpNow = this._viewportRect();
      const preBottom = preRect ? (preRect.bottom ?? (preRect.top + preRect.height)) : null;
      const preOnscreen = preRect && preBottom >= vpNow.top && preRect.top <= vpNow.bottom;
      if (!preOnscreen) {
        if (this.markActive && markAnchorNode != null) {
          sel.setBaseAndExtent(markAnchorNode, markAnchorOff, sel.focusNode, sel.focusOffset);
        }
        this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
        this._scrollToSelectionLineBounded();
        this._updateCursor();
        return true;
      }

      return this._finishMove(sel, true, markAnchorNode, markAnchorOff);
    }

    // Default (sentence, paragraph, etc.): sel.modify with guards
    let moved = this._stepModify(sel, direction, granularity, fwd);
    this._snapToText(sel, fwd);
    if (sel.focusNode === startNode && sel.focusOffset === startOff) { moved = false; }
    if (moved && granularity === "word" && !this._isPdfMode()) {
      this._snapToWordStart(sel, fwd);
      if (sel.focusNode === startNode && sel.focusOffset === startOff) { moved = false; }
    }

    if (moved && this._movedWrongWay(startNode, startOff, sel.focusNode, sel.focusOffset, fwd)) {
      if (this.markActive) sel.setBaseAndExtent(sel.anchorNode, sel.anchorOffset, startNode, startOff);
      else sel.collapse(startNode, startOff);
      moved = false;
    }

    // Fallback: void elements that sel.modify cannot cross
    if (!moved) {
      moved = this._fallbackToAdjacentText(sel, fwd, markAnchorNode, markAnchorOff);
    }

    return this._finishMove(sel, moved, markAnchorNode, markAnchorOff);
  }

  /** Common epilogue: restore mark, save caret, scroll & redraw. */
  _finishMove(sel, moved, anchorNode, anchorOff) {
    if (this.markActive && anchorNode != null) {
      sel.setBaseAndExtent(anchorNode, anchorOff, sel.focusNode, sel.focusOffset);
    }
    if (moved) {
      this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
      this._scrollToSelection();
      this._updateCursor();
    }
    return moved;
  }

  _stepModify(sel, direction, granularity, fwd) {
    const step = () => {
      const { focusNode: prevNode, focusOffset: prevOff } = sel;
      sel.modify("move", direction, granularity);
      if (!this._isContained(sel.focusNode)) {
        sel.collapse(prevNode, prevOff);
        return false;
      }
      if ((sel.focusNode !== prevNode || sel.focusOffset !== prevOff) &&
        this._movedWrongWay(prevNode, prevOff, sel.focusNode, sel.focusOffset, fwd)) {
        sel.collapse(prevNode, prevOff);
        return false;
      }
      this._unstickCaret(sel, direction, prevNode, prevOff);
      return sel.focusNode !== prevNode || sel.focusOffset !== prevOff;
    };
    let moved = step();
    while (moved && sel.focusNode.nodeType === Node.ELEMENT_NODE) {
      if (!step()) { break; }
    }
    return moved;
  }

  /** Nudge the caret past elements that sel.modify cannot cross. */
  _unstickCaret(sel, direction, oldNode, oldOff) {
    const focus = sel.focusNode, fwd = direction === "forward";
    // Case 1: inside an empty/void element — hop to parent edge.
    if (focus.nodeType === Node.ELEMENT_NODE && !focus.firstChild) {
      const parent = focus.parentNode;
      if (parent) {
        const idx = Array.from(parent.childNodes).indexOf(focus);
        sel.collapse(parent, fwd ? idx + 1 : idx);
      }
      return;
    }
    if (focus !== oldNode || sel.focusOffset !== oldOff) return; // moved — nothing to fix
    // Case 2: stuck at a text-node edge — walk to the next text node.
    if (focus.nodeType === Node.TEXT_NODE) {
      if (fwd && oldOff < focus.length) return;
      if (!fwd && oldOff > 0) return;
      const textNode = this._walkToVisible(focus, fwd);
      if (textNode) sel.collapse(textNode, fwd ? 0 : textNode.length);
      return;
    }
    // Case 3: stuck at an element position — step over adjacent empty child.
    if (focus.nodeType === Node.ELEMENT_NODE) {
      const adj = focus.childNodes[fwd ? oldOff : oldOff - 1];
      if (adj && adj.nodeType === Node.ELEMENT_NODE && !adj.textContent)
        sel.collapse(focus, fwd ? oldOff + 1 : oldOff - 1);
    }
  }

  /** Jump to the next/prev visible text node when sel.modify fails. */
  _fallbackToAdjacentText(sel, fwd, anchorNode = null, anchorOff = null) {
    const startNode = sel.focusNode.nodeType === Node.TEXT_NODE
      ? sel.focusNode
      : (sel.focusNode.childNodes[sel.focusOffset] || sel.focusNode);
    const textNode = this._walkToVisible(startNode, fwd);
    if (!textNode || textNode.nodeType !== Node.TEXT_NODE) return false;
    const offset = this._textVisibleEdgeOffset(textNode, fwd);
    if (anchorNode != null) sel.setBaseAndExtent(anchorNode, anchorOff, textNode, offset);
    else sel.collapse(textNode, offset);
    return true;
  }

  /* ── Boundary Detection ────────────────────────────────────── */

  _isAtVisibleBoundary(direction) {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return false;
    const node = sel.focusNode, offset = sel.focusOffset;
    const fwd = direction === "forward";
    if (node.nodeType !== Node.TEXT_NODE) return false;
    if (!this._isNavigableTextNode(node)) return !this._walkToVisible(node, fwd);
    if (fwd ? offset < node.length : offset > 0) return false;
    const hasNextVisible = !!this._walkToVisible(node, fwd);
    if (hasNextVisible) return false;
    // No more text in DOM, but in multi-page PDF, adjacent pages may
    // have unloaded text layers — not a true document boundary.
    const page = node.parentElement?.closest('.page[data-page-number]');
    const hasAdjPage = !!(page && this._visuallyAdjacentPage(page, fwd));
    if (hasAdjPage) return false;
    return true;
  }

  isAtBottom() {
    return Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight;
  }

  isAtTop() {
    return Math.floor(this._scrollTop) <= 0;
  }

  /* ── PDF Selection & Expansion ─────────────────────────────── */

  _detectSelectionScope() {
    const sel = window.getSelection();
    if (!sel?.rangeCount || sel.isCollapsed) return 'none';
    if (this.scrollContainer) {
      const pdfScope = this._selectionCoversPdfLines(sel);
      if (pdfScope) {
        return pdfScope;
      }
    }
    return /[.!?]|\S\s+\S/.test(sel.toString()) ? 'sentence' : 'word';
  }

  _selectionCoversPdfLines(sel) {
    const range = sel.getRangeAt(0);
    const startNode = range.startContainer;
    const endNode = range.endContainer;
    if (startNode.nodeType !== Node.TEXT_NODE || endNode.nodeType !== Node.TEXT_NODE) return null;

    const scopeRoot = this._pageScopeRoot(startNode);
    const { ordered, lines } = this._visuallyOrderedWithLines(scopeRoot);
    if (!ordered.length) return null;

    const selectedRange = this._selectedPdfLineRange(sel, lines);
    if (!selectedRange) return null;

    const { start, end } = selectedRange;
    if (start === end && !/\s/.test(sel.toString())) return null;

    const startBounds = this._lineBounds(lines[start]);
    const endBounds = this._lineBounds(lines[end]);
    if (!startBounds || !endBounds) return null;

    const coversWholeLines = range.startContainer === startBounds.first.node
      && range.startOffset === 0
      && range.endContainer === endBounds.last.node
      && range.endOffset === endBounds.last.node.length;
    if (!coversWholeLines) return null;

    const text = sel.toString();
    const trimmedText = text.trim();
    if (/[.!?]/.test(trimmedText) || end > start || /\n/.test(text)) return 'sentence';
    return 'word';
  }

  _selectedPdfLineRange(sel, lines) {
    if (!sel?.rangeCount || sel.isCollapsed) return null;
    const range = sel.getRangeAt(0);
    const find = (node) => {
      if (!lines?.length || node?.nodeType !== Node.TEXT_NODE) return -1;
      return lines.findIndex((line) => line.some((entry) => entry.node === node));
    };
    const startLine = find(range.startContainer);
    const endLine = find(range.endContainer);
    if (startLine < 0 || endLine < 0) return null;
    return {
      start: Math.min(startLine, endLine),
      end: Math.max(startLine, endLine)
    };
  }

  _pdfLineContext(sel) {
    const { node, offset } = this._resolveCursorPosition(sel.anchorNode, sel.anchorOffset);
    if (node.nodeType !== Node.TEXT_NODE || !this._isContained(node)) return null;

    const scopeRoot = this._pageScopeRoot(node);
    const { ordered, lines } = this._visuallyOrderedWithLines(scopeRoot);
    if (!ordered.length || !lines.length) return null;

    const caretRange = this._collapsedRange(node, offset);
    const caretRect = this._charRect(caretRange) || caretRange.getBoundingClientRect();
    if (!caretRect?.height) return null;

    const currentLine = this._findCaretLine(lines, caretRect);
    return currentLine < 0 ? null : { lines, currentLine };
  }

  _pdfAdjacentLineBlockCheck(currentLine, candidateLine, fwd, gapThreshold) {
    const current = this._lineBounds(currentLine);
    const candidate = this._lineBounds(candidateLine);
    if (!current || !candidate) return false;

    const minHeight = Math.min(current.height, candidate.height);

    if (Math.abs(candidate.left - current.left) > minHeight * 1.5) return false;

    const upperBounds = fwd ? current : candidate;
    const lowerBounds = fwd ? candidate : current;
    const gap = Math.max(0, lowerBounds.top - upperBounds.bottom);
    return gap <= (gapThreshold != null ? gapThreshold : minHeight * 1.2);
  }

  _expandPdfLineBlock(lines, startIdx, endIdx) {
    let start = startIdx;
    let end = endIdx;
    const gapThreshold = this._paragraphGapThreshold(lines);
    const refLeft = lines[startIdx]?.[0]?.rect?.left;

    const sameCol = (line) => {
      const first = line?.[0];
      return first && Math.abs(first.rect.left - refLeft) <= first.rect.height;
    };

    while (start > 0) {
      let cand = start - 1;
      while (cand > 0 && !sameCol(lines[cand])) cand--;
      if (cand < 0 || !sameCol(lines[cand])) break;
      if (!this._pdfAdjacentLineBlockCheck(lines[start], lines[cand], false, gapThreshold)) break;
      start = cand;
    }

    while (end < lines.length - 1) {
      let cand = end + 1;
      while (cand < lines.length - 1 && !sameCol(lines[cand])) cand++;
      if (cand >= lines.length || !sameCol(lines[cand])) break;
      if (!this._pdfAdjacentLineBlockCheck(lines[end], lines[cand], true, gapThreshold)) break;
      end = cand;
    }

    return { start, end };
  }

  _paragraphGapThreshold(lines) {
    if (lines.length < 3) return null;
    const gaps = [];
    for (let i = 0; i < lines.length - 1; i++) {
      const upper = this._lineBounds(lines[i]);
      const lower = this._lineBounds(lines[i + 1]);
      if (!upper || !lower) continue;
      if (Math.abs(lower.left - upper.left) > upper.height * 1.5) continue;
      gaps.push(Math.max(0, lower.top - upper.bottom));
    }
    if (gaps.length < 2) return null;
    gaps.sort((a, b) => a - b);

    let maxJump = 0, splitIdx = 0;
    for (let i = 0; i < gaps.length - 1; i++) {
      const jump = gaps[i + 1] - gaps[i];
      if (jump > maxJump) { maxJump = jump; splitIdx = i; }
    }
    if (maxJump <= gaps[0] * 0.5) return null;
    return (gaps[splitIdx] + gaps[splitIdx + 1]) / 2;
  }

  _pdfTextRangeModel(lines, startIdx, endIdx, columnLeft = null) {
    const blockLines = [];
    for (let i = startIdx; i <= endIdx; i++) {
      let entries = lines[i] || [];
      if (columnLeft !== null) {
        entries = entries.filter(e =>
          e.rect?.height && Math.abs(e.rect.left - columnLeft) <= e.rect.height
        );
      }
      if (entries.length > 0) {
        blockLines.push({ index: i, entries });
      }
    }
    const text = blockLines.map(({ entries }) =>
      entries.map((entry) => entry.node.textContent || '').join('')
    ).join('\n');

    return {
      text,
      offsetFromPoint: (node, offset) => {
        let total = 0;
        for (let idx = 0; idx < blockLines.length; idx++) {
          const { entries } = blockLines[idx];
          for (const entry of entries) {
            if (entry.node === node) {
              return total + Math.min(offset, entry.node.length);
            }
            total += entry.node.length;
          }
          if (idx < blockLines.length - 1) total += 1;
        }
        return total;
      },
      pointFromOffset: (offset) => {
        let remaining = Math.max(0, Math.min(offset, text.length));
        for (let idx = 0; idx < blockLines.length; idx++) {
          const { index, entries } = blockLines[idx];
          for (const entry of entries) {
            if (remaining <= entry.node.length) {
              return { node: entry.node, offset: remaining, line: index };
            }
            remaining -= entry.node.length;
          }
          if (idx < blockLines.length - 1) {
            if (remaining === 0) {
              const first = blockLines[idx + 1]?.entries[0]?.node;
              if (first) return { node: first, offset: 0, line: blockLines[idx + 1].index };
            }
            remaining = Math.max(0, remaining - 1);
          }
        }
        const lastLine = blockLines[blockLines.length - 1]?.entries;
        const lastNode = lastLine?.[lastLine.length - 1]?.node;
        return lastNode
          ? { node: lastNode, offset: lastNode.length, line: endIdx }
          : null;
      }
    };
  }

  _pdfSentenceOffsets(text, caretOffset) {
    const normalizedCaret = Math.max(0, Math.min(caretOffset, text.length));
    let start = 0;
    for (let idx = normalizedCaret - 1; idx >= 0; idx--) {
      if (/[.!?]/.test(text[idx])) {
        start = idx + 1;
        break;
      }
    }
    while (start < text.length && /[\s\n"'"\u201D\u2019)\]]/.test(text[start])) start++;

    let end = text.length;
    for (let idx = normalizedCaret; idx < text.length; idx++) {
      if (/[.!?]/.test(text[idx])) {
        end = idx + 1;
        while (end < text.length && /["'"\u201D\u2019)\]]/.test(text[end])) end++;
        break;
      }
    }
    while (end > start && /\s/.test(text[end - 1])) end--;

    return { start, end };
  }

  _expandPdfSentence(sel) {
    const context = this._pdfLineContext(sel);
    if (!context) {
      return false;
    }

    const { lines, currentLine } = context;
    const selectedRange = this._selectedPdfLineRange(sel, lines);
    const anchorStart = selectedRange ? Math.min(currentLine, selectedRange.start) : currentLine;
    const anchorEnd = selectedRange ? Math.max(currentLine, selectedRange.end) : currentLine;
    const { start, end } = this._expandPdfLineBlock(lines, anchorStart, anchorEnd);

    const caretPoint = this._resolveCursorPosition(sel.anchorNode, sel.anchorOffset);
    const caretEntry = context.lines[context.currentLine]
      ?.find(e => e.node === caretPoint.node);
    const columnLeft = caretEntry?.rect?.left ?? null;

    const model = this._pdfTextRangeModel(lines, start, end, columnLeft);
    const caretOffset = model.offsetFromPoint(caretPoint.node, caretPoint.offset);
    const offsets = this._pdfSentenceOffsets(model.text, caretOffset);

    const sp = model.pointFromOffset(offsets.start);
    const ep = model.pointFromOffset(offsets.end);
    if (!sp?.node || !ep?.node) return false;
    sel.setBaseAndExtent(ep.node, ep.offset, sp.node, sp.offset);

    this.markActive = true;
    return true;
  }

  _expandTo(sel, granularity) {
    if (this.scrollContainer) {
      if (granularity === 'sentenceboundary' && this._expandPdfSentence(sel)) return;
    }
    const range = sel.getRangeAt(0);
    const refNode = range.startContainer;
    const len = refNode.nodeType === Node.TEXT_NODE ? refNode.length : refNode.childNodes.length;
    const refOff = Math.min(range.startOffset + 1, len);
    sel.collapse(refNode, refOff);
    sel.modify('extend', 'backward', granularity);
    const startNode = sel.focusNode, startOff = sel.focusOffset;
    sel.collapse(refNode, refOff);
    sel.modify('extend', 'forward', granularity);
    const endNode = sel.focusNode, endOff = sel.focusOffset;
    sel.setBaseAndExtent(endNode, endOff, startNode, startOff);
    this.markActive = true;
  }

  /* ── Navigation ────────────────────────────────────────────── */

  _jumpToEdge(toStart, root) {
    const sel = this._ensureSelection();
    if (!sel) return;
    const scopeRoot = root || this._root;
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    let range;
    if (ordered.length) {
      const entry = toStart ? ordered[0] : ordered[ordered.length - 1];
      range = this._collapsedRange(entry.node, this._textVisibleEdgeOffset(entry.node, toStart));
    } else {
      range = document.createRange();
      range.selectNodeContents(scopeRoot);
      range.collapse(toStart);
    }
    this._applyRange(sel, range);
    if (root) this._scrollToSelection();
    else this._scrollTo(toStart ? 0 : this._scrollHeight);
    this._updateCursor();
  }

  /* ── Public API ────────────────────────────────────────────── */

  forward(granularity) {
    this._lastMoved = this._moveCaret("forward", granularity);
    this._lastDir = "forward";
  }

  backward(granularity) {
    this._lastMoved = this._moveCaret("backward", granularity);
    this._lastDir = "backward";
  }

  pageDown() { this._scrollPage("down"); }
  pageUp() { this._scrollPage("up"); }
  toggleMark() { this._setMark(!this.markActive); }

  deactivateMark() {
    this.markActive = false;
    const sel = window.getSelection();
    const c = this._savedFocus;
    if (c?.node && this._root.contains(c.node)) {
      sel.collapse(c.node, c.offset);
    } else if (sel?.rangeCount) {
      sel.collapse(sel.focusNode, sel.focusOffset);
      this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
    }
    setTimeout(() => this._restoreCaretIfLost(), 0);
    this._updateCursor();
  }

  expandSelection() {
    const sel = this._ensureSelection(true);
    if (!sel?.rangeCount) return;

    const scope = this._detectSelectionScope();
    if (scope === 'none') {
      this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
      sel.modify('move', 'forward', 'word');
      sel.modify('extend', 'backward', 'word');
      this.markActive = true;
    } else if (scope === 'word') {
      this._expandTo(sel, 'sentenceboundary');
    }
    this._scrollToSelection();
    this._updateCursor();
  }

  moveWithBoundaryCheck(method, granularity) {
    if (granularity) this[method](granularity);
    else this[method]();
    if (this._lastMoved) return "ok";
    if (this._lastDir === "forward" && (this._hitBoundary || this.isAtBottom())) return "at-end";
    if (this._lastDir === "backward" && (this._hitBoundary || this.isAtTop())) return "at-start";
    return "ok";
  }

  beginningOfBuffer() { this._jumpToEdge(true); }
  endOfBuffer() { this._jumpToEdge(false); }

  beginningOfPage() {
    const p = this._currentPage();
    if (p) this._jumpToEdge(true, p);
  }

  endOfPage() {
    const p = this._currentPage();
    if (p) this._jumpToEdge(false, p);
  }

  /** Simulate a mouse click at the current caret position. */
  clickAtCaret() {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return;
    const r = sel.getRangeAt(0).cloneRange();
    r.collapse(true);
    const rect = r.getBoundingClientRect();
    if (!rect?.height) return;
    const x = rect.left + rect.width / 2;
    const y = rect.top + rect.height / 2;
    const el = document.elementFromPoint(x, y);
    if (!el) return;
    el.dispatchEvent(new MouseEvent("click",
      { bubbles: true, cancelable: true, clientX: x, clientY: y }));
  }

  /** Remove all event listeners and the cursor overlay. */
  destroy() {
    document.removeEventListener("selectionchange", this._onSelectionChange);
    document.removeEventListener("keydown", this._onKeyDown);
    if (this.scrollContainer && this._onPdfScroll) {
      this.scrollContainer.removeEventListener("scroll", this._onPdfScroll);
    } else {
      window.removeEventListener("scroll", this._onScroll);
      window.removeEventListener("resize", this._onResize);
    }
    this._cursorEl?.remove();
    this._cursorEl = null;
  }
}

window.CaretEmacs = CaretEmacs;
const viewerContainer = document.getElementById('viewerContainer');
const viewer = document.getElementById('viewer');
if (viewerContainer && viewer) {
  window.__caretEmacs = new CaretEmacs(viewer, { scrollContainer: viewerContainer });
} else {
  window.__caretEmacs = new CaretEmacs(document);
}
