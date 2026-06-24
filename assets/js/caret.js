/** CaretEmacs – Emacs-like caret navigation for WebKit. */

/* ── Constants ────────────────────────────────────────────── */

const STYLE_ID = "__caret-emacs-style";
const CURSOR_TAG = "caret-cursor";
const WORD_CHAR_RE = /[\p{L}\p{N}\p{M}\p{Pc}'-]/u;

const cloneRect = (r) => ({ top: r.top, left: r.left, width: r.width, height: r.height });

const CURSOR_CSS = `
${CURSOR_TAG}{
position:absolute;pointer-events:none;z-index:2147483647;
background:var(--caret-color, #4488ff);display:none;box-sizing:border-box;
border-radius:1px;min-width:2px;
animation:caretBlink 1s step-end infinite;
}
@keyframes caretBlink{0%,100%{opacity:0.5}50%{opacity:0}}
`.trim();

class CaretEmacs {

  /* ── Construction ──────────────────────────────────────────── */

  constructor(el = document, opts = {}) {
    this.el = el;
    this.markActive = false;
    this._savedCaret = null;
    this._debug = false;
    this._debugLog = [];
    this._onSelectionChange = this._updateCursor.bind(this);
    this.scrollContainer = opts.scrollContainer || null;
    this._scrollPx = opts.scrollPx || 200;
    this._scrollDownFraction = opts.scrollDownFraction || 1 / 3;
    this._scrollUpFraction = opts.scrollUpFraction || 2 / 3;
    this._viewportEdgeOffset = opts.viewportEdgeOffset || 20;
    this._cursorEl = null;
    this._scrollRafPending = false;
    this._lastScrollTop = 0;
    this._suppressScrollRelocate = false;

    // Performance caches
    this._fontSizeCache = new WeakMap();
    this._textBoundsCache = new WeakMap();

    // Line move state preservation
    this._lineMoveTargetIndex = null;
    this._lineMoveGoalX = null;

    this._onKeyDown = (e) => {
      if (e.ctrlKey && e.key === 'g' && !e.shiftKey && !e.altKey && !e.metaKey)
        this.deactivateMark();
    };
    this._onScroll = () => this._onUserScroll();
    this._onResize = () => {
      this._suppressScrollRelocate = true;
      requestAnimationFrame(() => {
        this._suppressScrollRelocate = true;
        requestAnimationFrame(() => {
          this._lastRenderedPos = null;
          this._invalidateLayoutCaches();
          this._updateCursor();
          this._scrollToSelection();
        });
      });
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
    this._fontSizeCache = new WeakMap();
    this._textBoundsCache = new WeakMap();
  }

  /* ── Debug ─────────────────────────────────────────────────── */

  dumpDebug() {
    return this._debugLog.map((entry) => JSON.stringify(entry)).join("\n");
  }

  /** Dump page layout: mode, scroll, caret, visual lines with segments. */
  dumpPage() {
    const page = this._currentPage() || this.el;
    const { ordered, lines } = this._visuallyOrderedTextNodes(page);
    const caret = this._savedFocus;
    const vp = this._viewportRect();
    const mode = this._isPdfMode() ? "PDF" : "HTML";
    const r = [`=== dumpPage ===`,
      `mode: ${mode}  page: ${page?.dataset?.pageNumber || "-"}`,
      `viewport: ${Math.round(vp.left)},${Math.round(vp.top)} - ${Math.round(vp.right)},${Math.round(vp.bottom)}`,
      `scroll: ${Math.round(this._scrollTop)}/${Math.round(this._scrollHeight)} (${this.getScrollPercent().toFixed(1)}%)`,
      `mark: ${this.markActive}  caret: node=${caret?.node?.textContent?.slice(0, 10) || "-"} off=${caret?.offset ?? "-"}`,
      `lines: ${lines.length}  segments: ${ordered.length}`,
      `---`
    ];
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const bounds = this._lineBounds(line);
      const segs = line.map(e => {
        const x = Math.round(e.rect.left);
        const w = Math.round(e.rect.width);
        const h = Math.round(e.rect.height);
        const txt = (e.node.textContent || "").slice(0, 25).replace(/\n/g, "\\n");
        const grp = e.groupRoot ? `g` : `-`;
        const idx = ordered.indexOf(e);
        return `[${x}+${w}x${h} ${grp} #${idx} "${txt}"]`;
      }).join(" ");
      r.push(`L${i}: y=${Math.round(bounds.top)} x=${Math.round(bounds.left)}-${Math.round(bounds.right)} h=${Math.round(bounds.height)}  ${segs}`);
    }
    return r.join("\n");
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

  // Any scroll shifts the viewport, so cached client rects become stale
  _scrollBy(dy) {
    const c = this.scrollContainer;
    const before = c ? c.scrollTop : window.scrollY;
    if (c) c.scrollTop += dy;
    else window.scrollBy(0, dy);
    if ((c ? c.scrollTop : window.scrollY) !== before) this._invalidateLayoutCaches();
  }

  _scrollTo(y) {
    const c = this.scrollContainer;
    const before = c ? c.scrollTop : window.scrollY;
    if (c) c.scrollTop = y;
    else window.scrollTo(0, y);
    if ((c ? c.scrollTop : window.scrollY) !== before) this._invalidateLayoutCaches();
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
      if (!this._canScroll(true)) return false;
      const delta = Math.min(this._scrollPx, Math.max(0, bottom - viewportRect.bottom));
      if (delta <= 0) return false;
      this._scrollBy(delta);
      return true;
    }
    if (top < viewportRect.top) {
      if (!this._canScroll(false)) return false;
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
      if (!this._canScroll(true)) return false;
      // Scroll to place cursor at 1/3 from bottom of viewport
      const viewportHeight = this._viewportHeight;
      const delta = bottom - (viewportRect.top + viewportHeight * this._scrollDownFraction);
      if (delta <= 0) return false;
      this._scrollBy(delta);
      return true;
    } else if (top < viewportRect.top) {
      if (!this._canScroll(false)) return false;
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
    const isForward = direction === "down";
    const delta = this._viewportHeight / 3;
    this._suppressScrollRelocate = true;
    const dy = isForward ? delta : -delta;
    this._scrollBy(dy);
    // If cursor is still in viewport after scroll, keep it in place
    // rect is static so compute post-scroll position: screen shifts by -dy
    const newTop = rect.top - dy;
    const newBottom = rect.bottom - dy;
    const vp = this._viewportRect();
    if (newTop >= vp.top && newBottom <= vp.bottom) {
      this._updateCursor();
      return;
    }
    const cx = rect.left + rect.width / 2;
    const cy = isForward ? vp.top + this._viewportEdgeOffset : vp.bottom - this._viewportEdgeOffset;
    const range = this._probeWithFallback(cx, cy, isForward, vp);
    if (!range) return;
    this._applyProbeResult(range);
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
          this._invalidateLayoutCaches();
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

  /** Call callback on the next textlayerrendered event (or immediately if no event bus). */
  _onTextLayerReady(callback) {
    const bus = window.PDFViewerApplication?.eventBus;
    if (!bus) { callback(); return; }
    const on = () => { bus.off('textlayerrendered', on); this._invalidateLayoutCaches(); callback(); };
    bus.on('textlayerrendered', on);
  }

  _onUserScroll() {
    if (this._scrollRafPending) return;
    this._scrollRafPending = true;
    requestAnimationFrame(() => {
      this._scrollRafPending = false;
      this._invalidateLayoutCaches();
      const scrollTop = this._scrollTop;
      const isForward = scrollTop > this._lastScrollTop;
      // Scroll changed screen positions — force cursor redraw even if DOM position is same
      if (scrollTop !== this._lastScrollTop) this._lastRenderedPos = null;
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
      if (this._suppressScrollRelocate) {
        this._suppressScrollRelocate = false;
        this._updateCursor();
        return;
      }
      if (this._isRectInViewport(caretRect, viewportRect)) {
        this._updateCursor();
        return;
      }

      // Use caret's current X (still valid even if off-screen), or viewport center
      const cx = caretRect
        ? caretRect.left + caretRect.width / 2
        : (viewportRect.left + viewportRect.right) / 2;
      const cy = isForward ? viewportRect.top + this._viewportEdgeOffset : viewportRect.bottom - this._viewportEdgeOffset;

      let resolved = this._probeTextAt(cx, cy);
      // Reject probe result if it's outside the viewport
      if (resolved) {
        const probeRect = this._rangeRectAt(resolved.startContainer, resolved.startOffset);
        if (!this._isRectInViewport(probeRect, viewportRect)) resolved = null;
      }
      if (!resolved) {
        resolved = this._probeWithFallback(cx, cy, isForward, viewportRect);
      }
      if (resolved) {
        this._applyProbeResult(resolved);
      } else if (this._isPdfMode() && !this._pendingTextRetry) {
        // Text layer not rendered yet — retry once on next render
        this._pendingTextRetry = true;
        this._onTextLayerReady(() => {
          this._pendingTextRetry = false;
          const r = this._probeWithFallback(cx, cy, isForward, viewportRect);
          if (r) this._applyProbeResult(r);
          this._updateCursor();
        });
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
    this._cursorEl = document.querySelector(CURSOR_TAG)
      ?? (() => {
        const el = document.createElement(CURSOR_TAG);
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
      && el.style.display === "block") { return; }

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
    const rect = this._rangeRectAt(node, offset);
    if (rect) return rect;
    // Try previous char if it's on the same visual line
    if (offset > 0) {
      const prevRect = this._rangeRectAt(node, offset - 1);
      const cr = this._collapsedRange(node, offset).getBoundingClientRect();
      if (prevRect && cr && Math.abs(prevRect.top - cr.top) <= (prevRect.height || 10)) {
        return prevRect;
      }
    }
    // Final fallback: collapsed range position
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
    const rects = range.getClientRects();
    // Skip zero-width rects (line-break artifacts in pre-wrap) and take
    // the first rect with both height and width.
    for (let i = 0; i < rects.length; i++) {
      if (rects[i].height && rects[i].width) return rects[i];
    }
    const bcr = range.getBoundingClientRect();
    return (bcr?.height && bcr?.width) ? bcr : null;
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
      // When probing (preferFwd), walk forward first — stays on same visual
      // line. Backward walk can cross page boundaries to far-away text.
      const first = this._walkToVisible(node, preferFwd);
      if (first) return { node: first, offset: this._textVisibleEdgeOffset(first, preferFwd) };
      const second = this._walkToVisible(node, !preferFwd);
      if (second) return { node: second, offset: this._textVisibleEdgeOffset(second, !preferFwd) };
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
    if (!range) return null;
    if (!this._isContained(range.startContainer)) return null;
    if (this.scrollContainer) {
      const parentEl = range.startContainer.nodeType === Node.TEXT_NODE
        ? range.startContainer.parentElement : range.startContainer;
      if (!parentEl?.closest('.textLayer')) return null;
    }
    const resolved = this._rangeToText(range);
    if (!resolved) return null;
    const rr = this._rangeRectAt(resolved.startContainer, resolved.startOffset);
    if (rr && Math.abs(rr.top - cy) > rr.height * 5) return null;
    return resolved;
  }

  /** Find the first/last navigable text span in the visible viewport (PDF mode). */
  _findVisibleText(isForward, viewportRect, preferX) {
    const pages = this.el.querySelectorAll('.page');
    for (const page of pages) {
      const pageRect = page.getBoundingClientRect();
      if (pageRect.bottom < viewportRect.top || pageRect.top > viewportRect.bottom) continue;
      const textLayer = page.querySelector('.textLayer');
      if (!textLayer) continue;
      const spans = textLayer.querySelectorAll('span');
      let best = null, bestY = isForward ? Infinity : -Infinity, bestXDist = Infinity;
      for (const span of spans) {
        const spanRect = span.getBoundingClientRect();
        if (spanRect.bottom < viewportRect.top || spanRect.top > viewportRect.bottom) continue;
        const textNode = span.firstChild;
        if (textNode?.nodeType !== Node.TEXT_NODE || !this._isNavigableTextNode(textNode)) continue;
        const y = spanRect.top;
        const xDist = preferX != null ? Math.abs(spanRect.left - preferX) : 0;
        // Pick best Y; on Y tie, pick closest X to preferX
        const betterY = isForward ? y < bestY : y > bestY;
        const sameY = Math.abs(y - bestY) < (spanRect.height || 16);
        if (betterY || (sameY && xDist < bestXDist)) {
          bestY = y;
          bestXDist = xDist;
          best = textNode;
        }
      }
      if (best) {
        const off = isForward ? this._textVisibleEdgeOffset(best, true) : this._textVisibleEdgeOffset(best, false);
        return this._collapsedRange(best, off);
      }
    }
    return null;
  }

  /** Probe at (cx,cy); if nothing found and in PDF mode, fall back to _findVisibleText. */
  _probeWithFallback(cx, cy, isForward, viewportRect) {
    let range = this._probeTextAt(cx, cy);
    if (!range && this._isPdfMode()) {
      range = this._findVisibleText(isForward, viewportRect || this._viewportRect(), cx);
    }
    return range;
  }

  /** Apply a probed range to the selection and save focus. */
  _applyProbeResult(range) {
    const sel = window.getSelection();
    this._setSelectionRange(sel, range);
    this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
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
    const resolved = this._probeTextAt((viewportRect.left + viewportRect.right) / 2, viewportRect.top + this._viewportEdgeOffset);
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


  _applyRange(sel, range) {
    if (this.markActive) {
      sel.setBaseAndExtent(sel.anchorNode, sel.anchorOffset,
        range.startContainer, range.startOffset);
    } else {
      sel.removeAllRanges();
      sel.addRange(range);
    }
  }

  /** Apply mark selection from anchor to current focus.
   *  When extend is true, shifts the forward endpoint by +1 char so the
   *  character at cursor position is included (needed for end-of-line).
   *  Returns the real (pre-extension) focus for saving to _savedFocus. */
  _applyMarkSelection(sel, anchorNode, anchorOff, extend = false) {
    const focusNode = sel.focusNode;
    const focusOff = sel.focusOffset;
    if (extend) {
      const fwd = anchorNode === focusNode
        ? anchorOff < focusOff
        : !!(anchorNode.compareDocumentPosition(focusNode) & Node.DOCUMENT_POSITION_FOLLOWING);
      if (fwd && focusNode.nodeType === Node.TEXT_NODE) {
        sel.setBaseAndExtent(anchorNode, anchorOff, focusNode,
          Math.min(focusOff + 1, focusNode.textContent.length));
      } else {
        sel.setBaseAndExtent(anchorNode, anchorOff, focusNode, focusOff);
      }
    } else {
      sel.setBaseAndExtent(anchorNode, anchorOff, focusNode, focusOff);
    }
    return { node: focusNode, offset: focusOff };
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

  /** Build visual ordering and grouped lines for a scope root. */
  _visuallyOrderedTextNodes(root) {
    const tw = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = [];
    let domIndex = 0;
    while (tw.nextNode()) {
      const textNode = tw.currentNode;
      if (!this._isNavigableTextNode(textNode)) continue;
      const segments = this._splitNodeIntoLineSegments(textNode, domIndex++, root);
      for (const seg of segments) nodes.push(seg);
    }
    if (!nodes.length) return { ordered: [], lines: [] };

    // Cluster segments into vertical line-bands before sorting, so inline
    // boxes that share a visual line but differ in height/center (e.g. taller
    // <code> spans offset from surrounding prose) sort together by X. Sorting
    // by raw midY would reorder them and split one visual line into groups.
    const byMid = [...nodes].sort((a, b) =>
      (a.rect.top + a.rect.height / 2) - (b.rect.top + b.rect.height / 2));
    let band = -1, bandMid = NaN, bandH = 0;
    for (const n of byMid) {
      const mid = n.rect.top + n.rect.height / 2;
      if (band < 0 || Math.abs(mid - bandMid) > Math.max(n.rect.height, bandH) / 2) {
        band++; bandMid = mid; bandH = n.rect.height;
      }
      n._yBand = band;
    }
    nodes.sort((a, b) =>
      (a._yBand - b._yBand) ||
      (a.rect.left - b.rect.left) ||
      (a.domIndex - b.domIndex));

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

    return { ordered, lines };
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
          // Is this char on the next line or beyond?
          // Use the midpoint between current and next line centers as the
          // decision boundary. Chars past this boundary belong to the next
          // line or further; chars at or before it belong to the current line.
          const boundary = (lrMid + nextMid) / 2;
          if (charMid > boundary) {
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
      let sameColumn = true;
      if (sameVisualLine && sameGroup && currentLine.length > 0) {
        sameColumn = false;
        for (let j = 0; j < currentLine.length; j++) {
          const existing = currentLine[j];
          const gap = entry.rect.left - (existing.rect.left + existing.rect.width);
          const revGap = existing.rect.left - (entry.rect.left + entry.rect.width);
          const horizontalDist = Math.max(gap, revGap);
          if (horizontalDist <= Math.max(entry.rect.height, existing.rect.height)) {
            sameColumn = true;
            break;
          }
        }
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
    const lineRect = line[0]?.rect;
    if (!lineRect) return null;
    let bestRange = null, bestDist = Infinity;
    for (const entry of line) {
      const start = entry.startOffset ?? 0;
      const end = entry.endOffset ?? entry.node.length;
      for (let off = start; off < end; off++) {
        const r = document.createRange();
        r.setStart(entry.node, off); r.setEnd(entry.node, off + 1);
        const cr = r.getBoundingClientRect();
        if (!cr.width || !cr.height) continue;
        if (!this._isSameLine(cr, lineRect)) continue;
        const d = Math.abs(cr.left + cr.width / 2 - goalX);
        if (d < bestDist) {
          bestDist = d;
          bestRange = this._collapsedRange(entry.node, off);
        }
      }
    }
    return bestRange;
  }

  /** Find the segment containing the caret offset in the visual ordering.
   *  Returns { ordered, idx } or null. Shared by char/word movement. */
  _findCaretSegment(focus, focusOff, scopeRoot) {
    const { ordered } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    for (let i = 0; i < ordered.length; i++) {
      const e = ordered[i];
      if (e.node === focus && focusOff >= e.startOffset && focusOff <= e.endOffset) {
        return { ordered, idx: i };
      }
    }
    return null;
  }

  _lineBounds(line) {
    if (!line?.length) return null;
    const first = line[0];
    const last = line[line.length - 1];
    let top = first.rect.top;
    let bottom = first.rect.top + first.rect.height;
    let height = first.rect.height;
    let left = Infinity, right = -Infinity;
    for (const entry of line) {
      top = Math.min(top, entry.rect.top);
      bottom = Math.max(bottom, entry.rect.top + entry.rect.height);
      height = Math.max(height, entry.rect.height);
      left = Math.min(left, entry.rect.left);
      right = Math.max(right, entry.rect.left + entry.rect.width);
    }
    return { first, last, left, right, top, bottom, height: Math.max(height, bottom - top) };
  }

  /* ── Character & Word Movement ─────────────────────────────── */

  /** Unicode-aware word character classifier (handles CJK and other scripts). */
  _isWordChar(ch) {
    return Boolean(ch) && WORD_CHAR_RE.test(ch);
  }



  _moveCharVisual(sel, fwd) {
    const focus = sel.focusNode;
    const focusOff = sel.focusOffset;
    if (focus.nodeType !== Node.TEXT_NODE) return false;
    const text = focus.textContent;

    if (fwd) {
      if (focusOff < focus.length) {
        let step = 1;
        if (text.charCodeAt(focusOff) >= 0xD800 && text.charCodeAt(focusOff) <= 0xDBFF
          && focusOff + 1 < focus.length
          && text.charCodeAt(focusOff + 1) >= 0xDC00 && text.charCodeAt(focusOff + 1) <= 0xDFFF) {
          step = 2;
        }
        sel.collapse(focus, focusOff + step);
        return true;
      }
      // At end of node — cross to next distinct node via visual ordering
      const scopeRoot = this._pageScopeRoot(focus);
      const result = this._findCaretSegment(focus, focusOff, scopeRoot);
      if (!result) return false;
      const { ordered, idx } = result;
      let targetIdx = idx + 1;
      while (targetIdx < ordered.length && ordered[targetIdx].node === focus) targetIdx++;
      if (targetIdx >= ordered.length) return false;
      sel.collapse(ordered[targetIdx].node, 0);
      return true;
    } else {
      if (focusOff > 0) {
        let step = 1;
        if (text.charCodeAt(focusOff - 1) >= 0xDC00 && text.charCodeAt(focusOff - 1) <= 0xDFFF
          && focusOff - 2 >= 0
          && text.charCodeAt(focusOff - 2) >= 0xD800 && text.charCodeAt(focusOff - 2) <= 0xDBFF) {
          step = 2;
        }
        sel.collapse(focus, focusOff - step);
        return true;
      }
      // At start of node — cross to previous distinct node via visual ordering
      const scopeRoot = this._pageScopeRoot(focus);
      const result = this._findCaretSegment(focus, focusOff, scopeRoot);
      if (!result) return false;
      const { ordered, idx } = result;
      let targetIdx = idx - 1;
      while (targetIdx >= 0 && ordered[targetIdx].node === focus) targetIdx--;
      if (targetIdx < 0) return false;
      sel.collapse(ordered[targetIdx].node, ordered[targetIdx].endOffset);
      return true;
    }
  }

  /** Visual word movement (handles CJK, multi-segment nodes, Unicode word chars). */
  _moveWordVisual(sel, fwd) {
    const focus = sel.focusNode;
    const focusOff = sel.focusOffset;
    if (focus.nodeType !== Node.TEXT_NODE) return false;

    const scopeRoot = this._pageScopeRoot(focus);
    const result = this._findCaretSegment(focus, focusOff, scopeRoot);
    if (!result) return false;
    const { ordered, idx: curIdx } = result;

    if (fwd) {
      let node = focus, off = focusOff, idx = curIdx;
      while (off < node.textContent.length && this._isWordChar(node.textContent[off])) off++;
      while (true) {
        while (off < node.textContent.length && !this._isWordChar(node.textContent[off])) off++;
        if (off < node.textContent.length) {
          sel.collapse(node, off);
          return true;
        }
        do { idx++; } while (idx < ordered.length && ordered[idx].node === node);
        if (idx >= ordered.length) return false;
        node = ordered[idx].node;
        off = 0;
      }
    } else {
      let node = focus, off = focusOff, idx = curIdx;
      if (off > 0) {
        off--;
      } else {
        do { idx--; } while (idx >= 0 && ordered[idx].node === node);
        if (idx < 0) return false;
        node = ordered[idx].node;
        off = node.textContent.length - 1;
      }
      while (true) {
        while (off >= 0 && !this._isWordChar(node.textContent[off])) off--;
        if (off >= 0) break;
        do { idx--; } while (idx >= 0 && ordered[idx].node === node);
        if (idx < 0) return false;
        node = ordered[idx].node;
        off = node.textContent.length - 1;
      }
      while (off > 0 && this._isWordChar(node.textContent[off - 1])) off--;
      sel.collapse(node, off);
      return true;
    }
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

    // Loop: skip consecutive empty/whitespace-only lines (terminates at
    // text node boundary).
    while (targetStart >= 0 && targetEnd >= targetStart) {
      if (text.substring(targetStart, targetEnd).trim()) {
        const targetOff = Math.min(targetStart + column, targetEnd);
        return this._collapsedRange(node, targetOff);
      }
      if (fwd) {
        if (targetEnd >= len) return null;
        targetStart = targetEnd + 1;
        targetEnd = text.indexOf('\n', targetStart);
        if (targetEnd < 0) targetEnd = len;
      } else {
        if (targetStart <= 0) return null;
        targetEnd = targetStart - 1;
        const prevBreak = text.lastIndexOf('\n', Math.max(0, targetEnd - 1));
        targetStart = prevBreak + 1;
      }
    }
    return null;
  }

  /** DOM-based visual line movement. Returns { range, scrolled }. */
  _moveLine(fwd, cachedTargetLineIndex = null, cachedGoalX = null) {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return { range: null, scrolled: false };

    const { node: lineNode, offset: lineOffset } =
      this._resolveCursorPosition(sel.focusNode, sel.focusOffset, true);
    const startRange = this._collapsedRange(lineNode, lineOffset);
    const caretRect = this._lineMoveRect(lineNode, lineOffset) || this._charRect(startRange);
    if (!caretRect?.height) {
      return { range: null, scrolled: false };
    }
    const goalX = cachedGoalX !== null ? cachedGoalX : (caretRect.left + caretRect.width / 2);

    const preservedTextRange = this._moveWithinPreservedTextNode(lineNode, lineOffset, fwd);
    if (preservedTextRange) {
      return { range: preservedTextRange, scrolled: false };
    }

    // Phase 1: move within current scope (PDF page or full HTML body)
    const currentPage = this._currentPage();
    const scopeRoot = currentPage || this._root;
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return { range: null, scrolled: false };
    if (!lines.length) return { range: null, scrolled: false };

    let currentLineIndex, targetLineIndex;

    // Use cached target if available (continuing incremental scroll)
    if (cachedTargetLineIndex !== null && cachedTargetLineIndex >= 0 && cachedTargetLineIndex < lines.length) {
      targetLineIndex = cachedTargetLineIndex;
      // Still need currentLineIndex for gap detection
      const caretLineFound = this._findCaretLine(lines, caretRect);
      currentLineIndex = caretLineFound >= 0 ? caretLineFound : 0;
    } else {
      // Fresh move: find current line and compute target
      const caretLineFound = this._findCaretLine(lines, caretRect);
      const caretInGap = caretLineFound < 0;
      currentLineIndex = caretLineFound;
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
      if (currentLineIndex < 0) return { range: null, scrolled: false };
      targetLineIndex = this._lineTargetIndex(currentLineIndex, fwd, lines);

      // Check if we should scroll incrementally before jumping (only if target found)
      if (targetLineIndex >= 0) {
        const currentLineRect = lines[currentLineIndex][0].rect;
        const targetLineRect = lines[targetLineIndex][0].rect;
        const currentLineBottom = currentLineRect && (currentLineRect.bottom ?? (currentLineRect.top + currentLineRect.height));
        const targetLineBottom = targetLineRect && (targetLineRect.bottom ?? (targetLineRect.top + targetLineRect.height));
        if (currentLineRect && targetLineRect && currentLineBottom != null && targetLineBottom != null) {
          const lineGap = fwd
            ? targetLineRect.top - currentLineBottom
            : currentLineRect.top - targetLineBottom;
          const vp = this._viewportRect();
          const targetInViewport = targetLineRect.top < vp.bottom && targetLineBottom > vp.top;
          if (lineGap > this._scrollPx && !targetInViewport && !caretInGap) {
            if (this._canScroll(fwd)) {
              this._scrollBy(fwd ? this._scrollPx : -this._scrollPx);
              this._invalidateLayoutCaches();
              return { range: null, scrolled: true, targetLineIndex, goalX };
            }
            // At scroll boundary — fall through to jump
          }
        }
      }
    }

    // Target line found — merge adjacent line groups and pick position
    if (targetLineIndex >= 0) {
      // Merge entries from adjacent line groups at the same visual Y,
      // but only from the same column (horizontally overlapping with caret)
      const currBounds = this._lineBounds(lines[currentLineIndex]);
      const mergedLine = [...lines[targetLineIndex]];
      const targetLineRect = lines[targetLineIndex][0].rect;
      for (let j = targetLineIndex - 1; j >= 0; j--) {
        if (!this._isSameLine(lines[j][0].rect, targetLineRect)) break;
        const b = this._lineBounds(lines[j]);
        if (currBounds && b && (currBounds.right < b.left || b.right < currBounds.left)) continue;
        mergedLine.unshift(...lines[j]);
      }
      for (let j = targetLineIndex + 1; j < lines.length; j++) {
        if (!this._isSameLine(lines[j][0].rect, targetLineRect)) break;
        const b = this._lineBounds(lines[j]);
        if (currBounds && b && (currBounds.right < b.left || b.right < currBounds.left)) continue;
        mergedLine.push(...lines[j]);
      }
      return { range: this._pickPositionOnLine(mergedLine, goalX), scrolled: false };
    }

    // No target line in scope — scroll incrementally or cross page
    if (currentPage) {
      if (this._canScroll(fwd)) {
        this._scrollBy(fwd ? this._scrollPx : -this._scrollPx);
        this._invalidateLayoutCaches();
        // Check if adjacent page is now near viewport edge
        const adjacentPage = this._visuallyAdjacentPage(currentPage, fwd);
        if (adjacentPage) {
          const pageRect = adjacentPage.getBoundingClientRect();
          const vp = this._viewportRect();
          const pageNearEdge = fwd
            ? pageRect.top < vp.top + this._scrollPx
            : pageRect.bottom > vp.bottom - this._scrollPx;
          if (pageNearEdge) {
            // Adjacent page is close - cross to it
            return { range: this._moveLineCrossPage(currentPage, fwd, goalX), scrolled: false };
          }
        }
        // Scrolled but page not near yet - stop here (don't retry)
        return { range: null, scrolled: false };
      }
      // Can't scroll - cross to adjacent page
      return { range: this._moveLineCrossPage(currentPage, fwd, goalX), scrolled: false };
    }
    return this._scrollAndProbe(fwd, goalX, caretRect);
  }

  /**
   * Scroll this._scrollPx toward non-text content, then probe for a text line
   * that has entered the near viewport. Returns a Range or null.
   */
  _scrollAndProbe(fwd, goalX, caretRect) {
    if (!this._canScroll(fwd)) return { range: null, scrolled: false };

    this._scrollBy(fwd ? this._scrollPx : -this._scrollPx);
    this._invalidateLayoutCaches();

    const scopeRoot = this._currentPage() || this._root;
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
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
      const { ordered: adjacentOrdered, lines: adjacentLines } = this._visuallyOrderedTextNodes(adjacentPage);
      if (!adjacentOrdered.length) { skipped++; continue; }
      // Found text — scroll into view if needed, then pick line
      const adjacentRect = adjacentPage.getBoundingClientRect();
      const viewportRect = this._viewportRect();
      if (adjacentRect.bottom < viewportRect.top || adjacentRect.top > viewportRect.bottom) {
        adjacentPage.scrollIntoView({ block: fwd ? 'start' : 'end' });
        this._invalidateLayoutCaches();
      }
      if (!adjacentLines.length) { skipped++; continue; }
      const targetLine = fwd ? adjacentLines[0] : adjacentLines[adjacentLines.length - 1];
      return this._pickPositionOnLine(targetLine, goalX);
    }
    // Scroll first adjacent page into view to trigger text layer loading
    const firstAdjacentPage = this._visuallyAdjacentPage(page, fwd);
    if (firstAdjacentPage) {
      firstAdjacentPage.scrollIntoView({ block: fwd ? 'start' : 'end' });
      this._invalidateLayoutCaches();
    }
    return null;
  }

  /* ── Movement Dispatcher ───────────────────────────────────── */

  _moveCaret(direction, granularity) {
    const sel = this._ensureSelection(true);
    if (!sel) { return false; }

    // Capture mark anchor before any collapse that would lose it.
    const markAnchorNode = this.markActive ? sel.anchorNode : null;
    const markAnchorOff = this.markActive ? sel.anchorOffset : null;

    // Restore real cursor position (before mark extension) so movement
    // starts from the actual caret, not the extended selection endpoint.
    if (this._savedFocus && this.markActive && sel.rangeCount) {
      sel.collapse(this._savedFocus.node, this._savedFocus.offset);
    }

    this._hitBoundary = false;
    if (this._isAtVisibleBoundary(direction)) {
      this._hitBoundary = true;
      return false;
    }

    // Pre-snap: if on whitespace node, snap to visible text first
    const { node: snapNode, offset: snapOff } =
      this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    if (snapNode !== sel.focusNode || snapOff !== sel.focusOffset) {
      if (this.markActive) {
        sel.setBaseAndExtent(markAnchorNode, markAnchorOff, snapNode, snapOff);
      } else {
        sel.collapse(snapNode, snapOff);
      }
    }

    if (!this.markActive && !sel.isCollapsed) {
      sel.collapse(sel.focusNode, sel.focusOffset);
    }

    const fwd = direction === "forward";
    const startNode = sel.focusNode, startOff = sel.focusOffset;
    if (this.markActive) { sel.collapse(sel.focusNode, sel.focusOffset); }

    // PDF text layers and HTML both use visual ordering for char/word movement.
    if (granularity === "character" || granularity === "word") {
      let moved = granularity === "character"
        ? this._moveCharVisual(sel, fwd)
        : this._moveWordVisual(sel, fwd);
      // After expandSelection, _moveWordVisual lands at the start of the next word,
      // but the selection should include that entire word. Extend past it.
      if (moved && this._savedCaret && granularity === "word") {
        let n = sel.focusNode, o = sel.focusOffset;
        if (n.nodeType === Node.TEXT_NODE) {
          if (fwd) {
            while (o < n.textContent.length && this._isWordChar(n.textContent[o])) o++;
          } else {
            while (o > 0 && this._isWordChar(n.textContent[o - 1])) o--;
          }
          if (o !== sel.focusOffset) sel.collapse(n, o);
        }
      }
      if (moved) this._snapToText(sel, fwd);
      if (moved && this._movedWrongWay(startNode, startOff, sel.focusNode, sel.focusOffset, fwd)) {
        sel.collapse(startNode, startOff);
        moved = false;
      }
      return this._finishMove(sel, moved, markAnchorNode, markAnchorOff);
    }

    // Line: unified visual line movement for both PDF and HTML.
    if (granularity === "line") {
      const preRect = this._rangeRectAt(startNode, startOff)
        || this._collapsedRange(startNode, startOff).getBoundingClientRect();

      // Track target across incremental scroll iterations
      const cachedTargetLineIndex = this._lineMoveTargetIndex || null;
      const cachedGoalX = this._lineMoveGoalX || null;
      const result = this._moveLine(fwd, cachedTargetLineIndex, cachedGoalX);

      // If scrolled incrementally, cache target and retry
      if (result.scrolled) {
        // Only cache valid target indices (>= 0)
        if (result.targetLineIndex !== undefined && result.targetLineIndex >= 0) {
          this._lineMoveTargetIndex = result.targetLineIndex;
          this._lineMoveGoalX = result.goalX;
        }
        return this._moveCaret(direction, granularity, startNode, startOff, markAnchorNode, markAnchorOff);
      }

      // Clear cache on successful move or failure
      this._lineMoveTargetIndex = null;
      this._lineMoveGoalX = null;

      const lineRange = result.range;
      if (!lineRange) {
        const atVisibleBoundary = this._isAtVisibleBoundary(direction);
        const atViewportEdge = fwd ? this.isAtBottom() : this.isAtTop();
        this._hitBoundary = atVisibleBoundary || atViewportEdge;
        return false;
      }

      sel.removeAllRanges();
      sel.addRange(lineRange);

      // If caret was off-screen before move, use minimal scroll to reveal
      const vpNow = this._viewportRect();
      const preBottom = preRect ? (preRect.bottom ?? (preRect.top + preRect.height)) : null;
      const preOnscreen = preRect && preBottom >= vpNow.top && preRect.top <= vpNow.bottom;
      if (!preOnscreen) {
        this._savedFocus = this.markActive && markAnchorNode != null
          ? this._applyMarkSelection(sel, markAnchorNode, markAnchorOff)
          : { node: sel.focusNode, offset: sel.focusOffset };
        this._scrollToSelectionLineBounded();
        this._updateCursor();
        return true;
      }

      return this._finishMove(sel, true, markAnchorNode, markAnchorOff);
    }

    // Lineboundary: use visual line model for both PDF and HTML.
    // sel.modify("lineboundary") is unreliable in xwidget (goes to text node
    // boundary, produces zero-width rects at element edges).
    if (granularity === "lineboundary") {
      const scopeRoot = (this._isPdfMode() ? this._pageScopeRoot(sel.focusNode) : null) || this._root;
      const { lines } = this._visuallyOrderedTextNodes(scopeRoot);
      if (!lines.length) return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const caretRect = this._rangeRectAt(sel.focusNode, sel.focusOffset)
        || this._collapsedRange(sel.focusNode, sel.focusOffset).getBoundingClientRect();
      if (!caretRect?.height) return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const lineIdx = this._findCaretLine(lines, caretRect);
      if (lineIdx < 0) return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const line = lines[lineIdx];
      // Find the first/last character on this line that has a valid rect.
      let targetRange = null;
      if (fwd) {
        // End of line: scan backwards from last segment's end
        for (let si = line.length - 1; si >= 0 && !targetRange; si--) {
          const seg = line[si];
          for (let off = seg.endOffset - 1; off >= seg.startOffset; off--) {
            const r = this._rangeRectAt(seg.node, off);
            if (r) { targetRange = this._collapsedRange(seg.node, off); break; }
          }
        }
      } else {
        // Beginning of line: scan forwards from first segment's start
        for (let si = 0; si < line.length && !targetRange; si++) {
          const seg = line[si];
          for (let off = seg.startOffset; off < seg.endOffset; off++) {
            const r = this._rangeRectAt(seg.node, off);
            if (r) { targetRange = this._collapsedRange(seg.node, off); break; }
          }
        }
      }
      if (targetRange) {
        sel.removeAllRanges();
        sel.addRange(targetRange);
      }
      return this._finishMove(sel, !!targetRange, markAnchorNode, markAnchorOff,
        !!targetRange && fwd);
    }

    // PDF sentence: use visual line model to find [.!?。！？] boundaries.
    if (this._isPdfMode() && granularity === "sentence") {
      const { node, offset } = this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
      if (node.nodeType !== Node.TEXT_NODE || !this._isContained(node))
        return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const scopeRoot = this._pageScopeRoot(node);
      const { lines } = this._visuallyOrderedTextNodes(scopeRoot);
      if (!lines.length) return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const caretRange = this._collapsedRange(node, offset);
      const caretRect = this._charRect(caretRange) || caretRange.getBoundingClientRect();
      if (!caretRect?.height) return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const currentLine = this._findCaretLine(lines, caretRect);
      if (currentLine < 0) return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      const caretPoint = { node, offset };
      const columnLeft = this._lineBounds(lines[currentLine])?.left ?? null;
      const model = this._pdfTextRangeModel(lines, 0, lines.length - 1, columnLeft);
      const caretOffset = model.offsetFromPoint(caretPoint.node, caretPoint.offset);
      const probeOff = this._skipSentenceBoundary(model.text, caretOffset, fwd);
      const sentenceBounds = this._pdfSentenceOffsets(model.text, probeOff);
      const targetOffset = fwd ? sentenceBounds.end : sentenceBounds.start;
      const target = model.pointFromOffset(targetOffset);
      if (!target?.node || this._movedWrongWay(caretPoint.node, caretPoint.offset, target.node, target.offset, fwd))
        return this._finishMove(sel, false, markAnchorNode, markAnchorOff);
      sel.collapse(target.node, target.offset);
      return this._finishMove(sel, true, markAnchorNode, markAnchorOff);
    }

    // Default (sentence, etc.): sel.modify with guards
    let moved = this._stepModify(sel, direction, granularity, fwd);
    this._snapToText(sel, fwd);
    if (sel.focusNode === startNode && sel.focusOffset === startOff) { moved = false; }

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

  /** Common epilogue: restore mark, save caret, scroll & redraw.
   *  When extend is true (used by end-of-line), the selection endpoint is
   *  shifted +1 forward so the character at the cursor is included.
   *  _savedFocus always stores the real (pre-extension) offset. */
  _finishMove(sel, moved, anchorNode, anchorOff, extend = false) {
    if (this.markActive && anchorNode != null) {
      this._savedFocus = this._applyMarkSelection(sel, anchorNode, anchorOff, extend);
    }
    if (moved) {
      if (!this.markActive || anchorNode == null) {
        this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
      }
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

  _canScroll(fwd) {
    return fwd
      ? Math.ceil(this._scrollTop + this._viewportHeight) < this._scrollHeight
      : Math.floor(this._scrollTop) > 0;
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
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
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
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length || !lines.length) return null;

    const caretRange = this._collapsedRange(node, offset);
    const caretRect = this._charRect(caretRange) || caretRange.getBoundingClientRect();
    if (!caretRect?.height) return null;

    const currentLine = this._findCaretLine(lines, caretRect);
    return currentLine < 0 ? null : { lines, currentLine };
  }

  _isSameColumn(entries, columnLeft) {
    if (columnLeft == null) return true;
    const lineLeft = entries.reduce((min, e) =>
      e.rect?.height ? Math.min(min, e.rect.left) : min, Infinity);
    return lineLeft !== Infinity &&
      Math.abs(lineLeft - columnLeft) <= (entries[0]?.rect?.height ?? 10) * 2;
  }

  _pdfTextRangeModel(lines, startIdx, endIdx, columnLeft = null) {
    const blockLines = [];
    for (let i = startIdx; i <= endIdx; i++) {
      let entries = lines[i] || [];
      if (!this._isSameColumn(entries, columnLeft)) continue;
      entries = entries.filter(e => e.rect?.height);
      if (entries.length > 0) {
        blockLines.push({ index: i, entries });
      }
    }
    // Build text with paragraph-aware separators (\n\n for paragraph breaks)
    const lineTexts = blockLines.map(({ entries }) =>
      entries.map((entry) => entry.node.textContent || '').join('')
    );
    const sepLengths = [];
    let text = lineTexts[0] || '';
    for (let i = 0; i < blockLines.length - 1; i++) {
      const currTop = blockLines[i].entries[0]?.rect?.top ?? 0;
      const nextTop = blockLines[i + 1]?.entries[0]?.rect?.top ?? 0;
      const lh = blockLines[i].entries[0]?.rect?.height ?? 11;
      const isPara = (nextTop - currTop) > lh * 1.5;
      sepLengths.push(isPara ? 2 : 1);
      text += (isPara ? '\n\n' : '\n') + (lineTexts[i + 1] || '');
    }

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
          if (idx < blockLines.length - 1) total += sepLengths[idx];
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
            const sepLen = sepLengths[idx];
            if (remaining < sepLen) {
              const first = blockLines[idx + 1]?.entries[0]?.node;
              if (first) return { node: first, offset: 0, line: blockLines[idx + 1].index };
            }
            remaining = Math.max(0, remaining - sepLen);
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

  /** Skip past sentence-ending punctuation and whitespace in the given direction.
   *  Returns an offset inside the adjacent sentence, or the original offset. */
  _skipSentenceBoundary(text, offset, fwd) {
    const boundaryRe = /[\s\n.!?。！？]/;
    let off = offset;
    if (fwd) {
      while (off < text.length && boundaryRe.test(text[off])) off++;
    } else {
      while (off > 0 && boundaryRe.test(text[off - 1])) off--;
    }
    return off;
  }

  _pdfSentenceOffsets(text, caretOffset) {
    const off = Math.max(0, Math.min(caretOffset, text.length));
    const re = /[.!?。！？]|\n\n/g;
    let start = 0, end = text.length, m;
    while ((m = re.exec(text)) !== null) {
      if (m.index < off) start = m.index + m[0].length;
      else { end = m[0] === '\n\n' ? m.index : m.index + m[0].length; break; }
    }
    while (start < end && /[\s\n]/.test(text[start])) start++;
    while (end > start && /\s/.test(text[end - 1])) end--;
    return { start, end };
  }

  _expandPdfSentence(sel) {
    const context = this._pdfLineContext(sel);
    if (!context) {
      return false;
    }

    const { lines, currentLine } = context;

    const caretPoint = this._resolveCursorPosition(sel.anchorNode, sel.anchorOffset);
    const caretLineBounds = this._lineBounds(lines[currentLine]);
    const columnLeft = caretLineBounds?.left ?? null;

    const model = this._pdfTextRangeModel(lines, 0, lines.length - 1, columnLeft);
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
    const { ordered } = this._visuallyOrderedTextNodes(scopeRoot);
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
  toggleMark() {
    if (this.markActive) {
      this.deactivateMark();
      return false;
    }
    this.markActive = true;
    this._savedCaret = null;
    document.documentElement.style.setProperty("--caret-color", "#ff4444");
    this._updateCursor();
    return true;
  }

  deactivateMark() {
    this.markActive = false;
    document.documentElement.style.setProperty("--caret-color", "");
    const sel = window.getSelection();
    const c = this._savedCaret || this._savedFocus;
    this._savedCaret = null;
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
      this._savedCaret = { node: sel.focusNode, offset: sel.focusOffset };
      const node = sel.focusNode;
      const off = sel.focusOffset;
      if (node.nodeType === Node.TEXT_NODE) {
        const text = node.textContent;
        let start = off, end = off;
        // Extend start backward over word chars
        while (start > 0 && this._isWordChar(text[start - 1])) start--;
        // Extend end forward over word chars
        while (end < text.length && this._isWordChar(text[end])) end++;
        if (start === end) {
          // No word at caret — skip non-word chars and try again
          if (end < text.length) {
            while (end < text.length && !this._isWordChar(text[end])) end++;
            while (end < text.length && this._isWordChar(text[end])) end++;
            start = end;
            while (start > 0 && this._isWordChar(text[start - 1])) start--;
          } else {
            while (start > 0 && !this._isWordChar(text[start - 1])) start--;
            while (start > 0 && this._isWordChar(text[start - 1])) start--;
            end = off;
          }
        }
        sel.setBaseAndExtent(node, start, node, end);
        this._savedFocus = { node, offset: end };
      } else {
        sel.modify('move', 'forward', 'word');
        sel.modify('extend', 'backward', 'word');
      }
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

  _jumpToBufferEdge(isForward) {
    if (!this._isPdfMode()) return this._jumpToEdge(isForward);
    this._scrollTo(isForward ? 0 : this._scrollHeight);
    this._onTextLayerReady(() => {
      const vp = this._viewportRect();
      const y = isForward ? vp.top + 20 : vp.bottom - 20;
      const el = document.elementFromPoint((vp.left + vp.right) / 2, y);
      const page = el?.closest('.page[data-page-number]');
      if (page) this._jumpToEdge(isForward, page);
    });
  }

  beginningOfBuffer() { this._jumpToBufferEdge(true); }
  endOfBuffer() { this._jumpToBufferEdge(false); }

  getScrollPercent() {
    if (this._isPdfMode()) {
      const page = this._currentPage();
      if (page) {
        const pRect = page.getBoundingClientRect();
        const sel = window.getSelection();
        let y = -pRect.top;
        if (sel?.focusNode) {
          const rect = this._rangeRectAt(sel.focusNode, sel.focusOffset)
            || this._collapsedRange(sel.focusNode, sel.focusOffset).getBoundingClientRect();
          if (rect) y = rect.top - pRect.top;
        }
        const total = pRect.height;
        return total > 0 ? Math.min(100, Math.max(0, y / total * 100)) : 0;
      }
    }
    const total = this._scrollHeight - this._viewportHeight;
    let y = this._scrollTop;
    const sel = window.getSelection();
    if (sel?.focusNode) {
      const rect = this._rangeRectAt(sel.focusNode, sel.focusOffset)
        || this._collapsedRange(sel.focusNode, sel.focusOffset).getBoundingClientRect();
      if (rect) y = rect.top + this._scrollTop;
    }
    return total > 0 ? Math.min(100, y / total * 100) : 0;
  }

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

  caretInfo() {
    const s = window.getSelection();
    const c = this._savedCaret || this._savedFocus || { node: s.focusNode, offset: s.focusOffset };
    // Collapsed range at the saved caret
    let rc = this._collapsedRange(c.node, c.offset).getBoundingClientRect();
    if (!rc || (!rc.width && !rc.height))
      rc = this._cursorRectAt(c.node, c.offset);
    const left = rc?.left ?? 0, bottom = rc?.bottom ?? 0;
    let text = "";
    if (!s.isCollapsed) {
      text = s.toString();
      this.deactivateMark();
    } else {
      const n = s.focusNode, o = s.focusOffset;
      if (n?.nodeType === 3) {
        const t = n.textContent;
        let b = o;
        while (b > 0 && this._isWordChar(t[b - 1])) b--;
        const m = t.slice(b).match(new RegExp('^' + WORD_CHAR_RE.source + '+', 'u'));
        if (m) text = m[0];
      }
    }
    return JSON.stringify({ text, left, bottom });
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
  window.__caretEmacs?.destroy();
  window.__caretEmacs = new CaretEmacs(viewer, { scrollContainer: viewerContainer });
} else {
  window.__caretEmacs?.destroy();
  window.__caretEmacs = new CaretEmacs(document);
}
