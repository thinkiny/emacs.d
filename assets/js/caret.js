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
    this._markAnchor = null;
    this._savedCaret = null;
    this._debug = false;
    this._debugLog = [];
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
    this._visualOrderCache = { root: null, layoutGeneration: -1, ordered: null, lines: null };
    this._layoutGeneration = 0;

    // Line move state preservation
    this._lineMoveTargetIndex = null;
    this._lineMoveGoalX = null;

    this._onSelectionChange = this._updateCursor.bind(this);
    this._onKeyDown = (e) => {
      if (e.ctrlKey && e.key === 'g' && !e.altKey && !e.metaKey)
        this.deactivateMark();
    };
    // A mouse click quits the mark (Emacs down-mouse semantics).
    this._onMouseDown = () => {
      if (this.markActive) this.deactivateMark();
    };
    this._onScroll = () => this._onUserScroll();
    this._onResize = () => {
      // Capture pre-reflow viewport-relative Y before a reflow-triggered
      // scroll event overwrites _caretViewportTop.
      const targetRelTop = this._caretViewportTop;
      this._suppressScrollRelocate = true;
      this._lastScrollTop = this._scrollTop;
      requestAnimationFrame(() => {
        this._suppressScrollRelocate = true;
        requestAnimationFrame(() => {
          this._lastRenderedPos = null;
          this._invalidateLayoutCaches();
          // Anchor to the caret's pre-reflow viewport-relative Y; fall back
          // to a bounded reveal.
          let anchored = false;
          if (targetRelTop != null) {
            const sel = window.getSelection();
            const caretRect = sel?.rangeCount ? this._selectionFocusRect(sel) : null;
            if (caretRect) {
              const delta = (caretRect.top - this._viewportRect().top) - targetRelTop;
              if (Math.abs(delta) >= 1) this._scrollBy(delta);
              anchored = true;
            }
          }
          if (!anchored) this._scrollToSelectionLineBounded();
          this._updateCursor();
          // Re-arm: a resize can clamp scrollY and fire a stray scroll event.
          this._suppressScrollRelocate = true;
          this._lastScrollTop = this._scrollTop;
        });
      });
    };

    const init = () => {
      this._initCursor();
      document.addEventListener("selectionchange", this._onSelectionChange);
      document.addEventListener("keydown", this._onKeyDown);
      document.addEventListener("mousedown", this._onMouseDown);
      if (this.scrollContainer) {
        this._initPdfScroll();
      } else {
        // Reload builds a new DOM; restore the persisted caret and keep
        // browser scroll restoration from fighting ours.
        if ('scrollRestoration' in history) history.scrollRestoration = 'manual';
        if (!this._restoreCaret()) this._ensureSelection();
        this._updateCursor();
        window.addEventListener('scroll', this._onScroll, { passive: true });
      }
      // resize fires on window in both modes (elements need ResizeObserver).
      window.addEventListener('resize', this._onResize, { passive: true });
    };
    document.body ? init() : document.addEventListener("DOMContentLoaded", init, { once: true });
  }

  get _root() { return this.el === document ? document.body : this.el; }

  _invalidateLayoutCaches() {
    this._fontSizeCache = new WeakMap();
    this._textBoundsCache = new WeakMap();
    this._layoutGeneration++;
  }

  /* ── Debug ─────────────────────────────────────────────────── */

  dumpDebug() {
    return this._debugLog.map((entry) => JSON.stringify(entry)).join("\n");
  }

  enableDebug(enabled) { this._debug = !!enabled; }

  clearDebug() { this._debugLog = []; }

  /** Dump page layout: mode, scroll, caret, visual lines with segments. */
  dumpPage() {
    const page = this._currentPage() || this.el;
    const { ordered, lines } = this._visuallyOrderedTextNodes(page);
    const caret = this._savedFocus;
    const viewport = this._viewportRect();
    const mode = this._isPdfMode() ? "PDF" : "HTML";
    const report = [`=== dumpPage ===`,
      `mode: ${mode}  page: ${page?.dataset?.pageNumber || "-"}`,
      `viewport: ${Math.round(viewport.left)},${Math.round(viewport.top)} - ${Math.round(viewport.right)},${Math.round(viewport.bottom)}`,
      `scroll: ${Math.round(this._scrollTop)}/${Math.round(this._scrollHeight)} (${this.getScrollPercent().toFixed(1)}%)`,
      `mark: ${this.markActive}  caret: node=${caret?.node?.textContent?.slice(0, 10) || "-"} off=${caret?.offset ?? "-"}`,
      `lines: ${lines.length}  segments: ${ordered.length}`,
      `---`
    ];
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const bounds = this._lineBounds(line);
      const segs = line.map(entry => {
        const x = Math.round(entry.rect.left);
        const w = Math.round(entry.rect.width);
        const h = Math.round(entry.rect.height);
        const txt = (entry.node.textContent || "").slice(0, 25).replace(/\n/g, "\\n");
        const grp = entry.groupRoot ? `g` : `-`;
        const idx = ordered.indexOf(entry);
        return `[${x}+${w}x${h} ${grp} #${idx} "${txt}"]`;
      }).join(" ");
      report.push(`L${i}: y=${Math.round(bounds.top)} x=${Math.round(bounds.left)}-${Math.round(bounds.right)} h=${Math.round(bounds.height)}  ${segs}`);
    }
    return report.join("\n");
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
    const container = this.scrollContainer;
    const before = container ? container.scrollTop : window.scrollY;
    if (container) container.scrollTop += dy;
    else window.scrollBy(0, dy);
    if ((container ? container.scrollTop : window.scrollY) !== before) this._invalidateLayoutCaches();
  }

  _scrollTo(y) {
    const container = this.scrollContainer;
    const before = container ? container.scrollTop : window.scrollY;
    if (container) container.scrollTop = y;
    else window.scrollTo(0, y);
    if ((container ? container.scrollTop : window.scrollY) !== before) this._invalidateLayoutCaches();
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
    // Under mark, getRangeAt(0) is the region bbox — judge visibility by the
    // real caret (_savedFocus) so an on-screen caret isn't relocated eagerly.
    let rect;
    if (this.markActive && this._savedFocus && this._isContained(this._savedFocus.node)) {
      rect = this._rangeRectAt(this._savedFocus.node, this._savedFocus.offset)
        || this._collapsedRange(this._savedFocus.node, this._savedFocus.offset).getBoundingClientRect();
    } else {
      rect = sel.getRangeAt(0).getBoundingClientRect();
    }
    if (!rect) return;
    const isForward = direction === "down";
    const delta = this._viewportHeight / 3;
    this._suppressScrollRelocate = true;
    const deltaY = isForward ? delta : -delta;
    this._scrollBy(deltaY);
    // If cursor is still in viewport after scroll, keep it in place
    // rect is static so compute post-scroll position: screen shifts by -dy
    const newTop = rect.top - deltaY;
    const newBottom = rect.bottom - deltaY;
    const viewport = this._viewportRect();
    if (newTop >= viewport.top && newBottom <= viewport.bottom) {
      this._updateCursor();
      return;
    }
    const probeX = rect.left + rect.width / 2;
    const probeY = isForward ? viewport.top + this._viewportEdgeOffset : viewport.bottom - this._viewportEdgeOffset;
    const range = this._probeWithFallback(probeX, probeY, isForward, viewport);
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
      const probeX = caretRect
        ? caretRect.left + caretRect.width / 2
        : (viewportRect.left + viewportRect.right) / 2;
      const probeY = isForward ? viewportRect.top + this._viewportEdgeOffset : viewportRect.bottom - this._viewportEdgeOffset;

      let resolved = this._probeTextAt(probeX, probeY);
      // Reject probe result if it's outside the viewport
      if (resolved) {
        const probeRect = this._rangeRectAt(resolved.startContainer, resolved.startOffset);
        if (!this._isRectInViewport(probeRect, viewportRect)) resolved = null;
      }
      if (!resolved) {
        resolved = this._probeWithFallback(probeX, probeY, isForward, viewportRect);
      }
      if (resolved) {
        this._applyProbeResult(resolved);
      } else if (this._isPdfMode() && !this._pendingTextRetry) {
        // Text layer not rendered yet — retry once on next render
        this._pendingTextRetry = true;
        this._onTextLayerReady(() => {
          this._pendingTextRetry = false;
          const range = this._probeWithFallback(probeX, probeY, isForward, viewportRect);
          if (range) this._applyProbeResult(range);
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
    // The overlay can be stripped by page hydration; re-attach before drawing.
    if (!el.isConnected) document.documentElement.appendChild(el);
    const sel = window.getSelection();
    if (!sel?.rangeCount) {
      // Selection lost — restore from the last rendered position.
      const saved = this._lastRenderedPos;
      if (saved?.node && this._root.contains(saved.node)) {
        sel.removeAllRanges();
        sel.addRange(this._collapsedRange(saved.node, saved.offset));
      }
      if (!sel?.rangeCount) { el.style.display = "none"; return; }
    }
    // Skip redundant redraw if position unchanged
    const lastRendered = this._lastRenderedPos;
    if (lastRendered && sel.focusNode === lastRendered.node && sel.focusOffset === lastRendered.offset
      && el.style.display === "block") { return; }

    if (!this._isContained(sel.focusNode)) { el.style.display = "none"; return; }
    // Render at the actual focus when it has a usable rect — whitespace-only
    // nodes are real positions; resolving them backward lands on the prior
    // line and makes moves look stuck.
    let node = sel.focusNode, offset = sel.focusOffset;
    let rect = node.nodeType === Node.TEXT_NODE ? this._cursorRectAt(node, offset) : null;
    if (!rect?.height) {
      const resolved = this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
      node = resolved.node; offset = resolved.offset;
      rect = this._cursorRectAt(node, offset);
    }
    if (!rect?.height) { el.style.display = "none"; return; }
    const cursorWidth = this._cursorWidth(rect);
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
    el.style.cssText = `display:block;left:${cursorLeft}px;top:${cursorTop}px;width:${cursorWidth}px;height:${cursorHeight}px`;
    // Save current focus as last-rendered position
    this._lastRenderedPos = { node: sel.focusNode, offset: sel.focusOffset };
    // Track viewport-relative Y for resize re-anchoring.
    this._caretViewportTop = rect.top - this._viewportRect().top;
    // Persist the caret across reloads (HTML only).
    if (!this.scrollContainer) this._persistCaret();
  }

  /** Get a client rect for cursor display at the given text position. */
  _cursorRectAt(node, offset) {
    const rect = this._rangeRectAt(node, offset);
    if (rect) return rect;
    // Try previous char if it's on the same visual line
    if (offset > 0) {
      const prevRect = this._rangeRectAt(node, offset - 1);
      const charRect = this._collapsedRange(node, offset).getBoundingClientRect();
      if (prevRect && charRect && Math.abs(prevRect.top - charRect.top) <= (prevRect.height || 10)) {
        return prevRect;
      }
    }
    // Final fallback: collapsed range position
    const charRect = this._collapsedRange(node, offset);
    return charRect.getClientRects()[0] || charRect.getBoundingClientRect();
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
    // Skip zero-width line-break artifacts in pre-wrap.
    for (let i = 0; i < rects.length; i++) {
      if (rects[i].height && rects[i].width) return rects[i];
    }
    const boundingRect = range.getBoundingClientRect();
    return (boundingRect?.height && boundingRect?.width) ? boundingRect : null;
  }

  /** True when two rects share the same visual line. */
  _isSameLine(rectA, rectB) {
    const tolerance = Math.max(rectA.height, rectB.height) / 2;
    return Math.abs((rectA.top + rectA.height / 2) - (rectB.top + rectB.height / 2)) <= tolerance;
  }

  /** Return a TreeWalker for text nodes, positioned at `node`. */
  _textWalker(node) {
    const walker = document.createTreeWalker(this._root, NodeFilter.SHOW_TEXT);
    walker.currentNode = node;
    return walker;
  }

  /** Walk from `node` in direction `fwd`, returning the first visible text node, or null. */
  _walkToVisible(node, fwd) {
    if (!this._isContained(node)) return null;
    const walker = this._textWalker(node);
    let textNode = fwd ? walker.nextNode() : walker.previousNode();
    while (textNode && !(this._isNavigableTextNode(textNode) && this._hasRenderedBox(textNode))) {
      textNode = fwd ? walker.nextNode() : walker.previousNode();
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

  /** True if the text node is rendered content — script/style text has no
   *  visual box and produces broken selections. */
  _isNavigableTextNode(textNode) {
    if (textNode?.nodeType !== Node.TEXT_NODE) return false;
    const text = textNode.textContent || "";
    if (!text.length) return false;
    for (let el = textNode.parentElement; el; el = el.parentElement) {
      switch (el.tagName) {
        case "SCRIPT": case "STYLE": case "NOSCRIPT": case "TEMPLATE": case "TITLE":
          return false;
        default: break;
      }
    }
    return this._preservesWhitespace(textNode) || !!text.trim();
  }

  /** True if the text node currently has a layout box — false inside
   *  display:none / hidden ancestors, SVG <title>/<desc>, aria-hidden. */
  _hasRenderedBox(textNode) {
    if (!textNode || textNode.nodeType !== Node.TEXT_NODE) return false;
    const range = document.createRange();
    range.selectNodeContents(textNode);
    const rects = range.getClientRects();
    for (let i = 0; i < rects.length; i++) {
      if (rects[i].width && rects[i].height) return true;
    }
    return false;
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
      const textNode = this._walkToVisible(start, true) || this._walkToVisible(start, false);
      if (textNode) return { node: textNode, offset: this._textVisibleEdgeOffset(textNode, true) };
    }
    if (node.nodeType === Node.TEXT_NODE &&
      (!this._isNavigableTextNode(node) || !this._hasRenderedBox(node))) {
      // When probing (preferFwd), walk forward first — backward can cross
      // page boundaries to far-away text.
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

  /** Nearest position:fixed/sticky ancestor of a node, or null — keeps the
   *  scroll probe out of pinned chrome at the viewport edge. */
  _pinnedAncestor(node) {
    let el = node?.nodeType === Node.ELEMENT_NODE ? node : node?.parentElement;
    for (; el && el !== this._root; el = el.parentElement) {
      const pos = getComputedStyle(el).position;
      if (pos === 'fixed' || pos === 'sticky') return el;
    }
    return null;
  }

  /** Probe for a text range at screen coordinates; returns a collapsed Range or null. */
  _probeTextAt(cx, cy) {
    let range = document.caretRangeFromPoint(cx, cy);
    if (!range) return null;
    // Step below a pinned header that overlaps the probe point; guard
    // against stacked bars.
    let effectiveCy = cy;
    for (let guard = 0; guard < 3; guard++) {
      const pinned = this._pinnedAncestor(range.startContainer);
      if (!pinned) break;
      effectiveCy = pinned.getBoundingClientRect().bottom + 1;
      const next = document.caretRangeFromPoint(cx, effectiveCy);
      if (!next || next.startContainer === range.startContainer) return null;
      range = next;
    }
    if (!this._isContained(range.startContainer)) return null;
    if (this.scrollContainer) {
      const parentEl = range.startContainer.nodeType === Node.TEXT_NODE
        ? range.startContainer.parentElement : range.startContainer;
      if (!parentEl?.closest('.textLayer')) return null;
    }
    const resolved = this._rangeToText(range);
    if (!resolved) return null;
    const rangeRect = this._rangeRectAt(resolved.startContainer, resolved.startOffset);
    if (rangeRect && Math.abs(rangeRect.top - effectiveCy) > rangeRect.height * 5) return null;
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

  /** The mark point with its offset clamped to the node's current length, or
   *  null if the anchor node left the tree. Guards setBaseAndExtent (which
   *  does not clamp) against a stale offset under DOM mutation. */
  _markAnchorPoint() {
    const anchor = this._markAnchor;
    if (!anchor || !this._isContained(anchor.node)) return null;
    const max = anchor.node.nodeType === Node.TEXT_NODE ? anchor.node.length : anchor.node.childNodes.length;
    return { node: anchor.node, offset: Math.min(anchor.offset, max) };
  }

  /** Apply a probed range to the selection and save focus. Under mark, the
   *  region is re-rendered from state (mark ↔ point). */
  _applyProbeResult(range) {
    const sel = window.getSelection();
    this._setSelectionRange(sel, range);
    this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
    const mark = this.markActive ? this._markAnchorPoint() : null;
    if (mark) {
      sel.setBaseAndExtent(mark.node, mark.offset, sel.focusNode, sel.focusOffset);
    }
    this._logDebug("probe-apply", {
      rangeCount: sel?.rangeCount ?? 0,
      markActive: this.markActive
    });
  }

  /** Probe one visual line below/above a caret rect (fallback for _moveLine). */
  _probeLineFromCaret(fwd, caretRect) {
    if (!caretRect) return null;
    const step = (caretRect.height || 18) + 6;
    const probeX = caretRect.left + (caretRect.width || 1) / 2;
    const probeY = fwd ? caretRect.bottom + step : caretRect.top - step;
    return this._probeTextAt(probeX, probeY);
  }

  /* ── Selection Management ──────────────────────────────────── */

  _ensureSelection(skipRelocate = false) {
    const sel = window.getSelection();
    if (sel?.rangeCount > 0) {
      if (!skipRelocate) this._relocateIfOffscreen(sel);
      return sel;
    }
    const saved = this._savedFocus;
    if (saved?.node && this._root.contains(saved.node)) {
      const range = this._collapsedRange(saved.node, saved.offset);
      sel.removeAllRanges();
      sel.addRange(range);
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
      this._applyProbeResult(resolved);
    }
  }

  /* ── Cross-reload position persistence (HTML) ──────────────────
   *  Persist a global character offset (stable across reload) in
   *  localStorage keyed by URL; resolve it back on the next load. Entries
   *  are never auto-evicted; delete by hand. */

  _caretStorageKey() {
    const url = location.href.split(/[?#]/)[0];         // strip ?query / #hash
    return `caret-pos:${url}`;
  }

  /** Count characters in navigable text nodes before (node, offset). */
  _caretGlobalOffset(node, offset) {
    if (node?.nodeType !== Node.TEXT_NODE) return null;
    const walker = document.createTreeWalker(this._root, NodeFilter.SHOW_TEXT);
    let total = 0;
    while (walker.nextNode()) {
      const textNode = walker.currentNode;
      if (textNode === node) return total + Math.min(offset, textNode.length);
      if (this._isNavigableTextNode(textNode)) total += textNode.length;
    }
    return null;
  }

  /** Resolve a global character offset back to a { node, offset }, or null. */
  _resolveGlobalOffset(target) {
    const walker = document.createTreeWalker(this._root, NodeFilter.SHOW_TEXT);
    let total = 0;
    while (walker.nextNode()) {
      const textNode = walker.currentNode;
      if (!this._isNavigableTextNode(textNode)) continue;
      if (total + textNode.length >= target) return { node: textNode, offset: target - total };
      total += textNode.length;
    }
    return null;
  }

  _persistCaret() {
    const sel = window.getSelection();
    const caret = this._savedFocus
      || (sel?.focusNode ? { node: sel.focusNode, offset: sel.focusOffset } : null);
    if (!caret?.node || !this._isContained(caret.node)) return;
    const offset = this._caretGlobalOffset(caret.node, caret.offset);
    if (offset == null) return;
    try { localStorage.setItem(this._caretStorageKey(), String(offset)); } catch (e) { }
  }

  /** Restore the persisted caret. Returns true if a position was applied. */
  _restoreCaret() {
    let stored;
    try { stored = localStorage.getItem(this._caretStorageKey()); } catch (e) { return false; }
    if (stored == null) return false;
    const target = parseInt(stored, 10);
    if (!Number.isFinite(target)) return false;
    const position = this._resolveGlobalOffset(target);
    if (!position) return false;
    const sel = window.getSelection();
    this._setSelectionRange(sel, this._collapsedRange(position.node, position.offset));
    this._savedFocus = { node: position.node, offset: position.offset };
    this._scrollToSelection();
    return true;
  }


  _applyRange(sel, range) {
    const mark = this.markActive ? this._markAnchorPoint() : null;
    if (mark) {
      sel.setBaseAndExtent(mark.node, mark.offset,
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
    const viewport = this._viewportRect();
    const probeX = (viewport.left + viewport.right) / 2;
    const probeY = (viewport.top + viewport.bottom) / 2;
    return document.elementFromPoint(probeX, probeY)?.closest('.page[data-page-number]') || null;
  }

  _visuallyAdjacentPage(currentPage, fwd) {
    const pages = Array.from(this._root.querySelectorAll('.page[data-page-number]'));
    if (pages.length < 2) return null;
    pages.sort((a, b) => {
      const ra = a.getBoundingClientRect(), rb = b.getBoundingClientRect();
      const deltaY = ra.top - rb.top;
      return Math.abs(deltaY) > 5 ? deltaY : ra.left - rb.left;
    });
    const idx = pages.indexOf(currentPage);
    if (idx === -1) return null;
    const adjIdx = fwd ? idx + 1 : idx - 1;
    return (adjIdx >= 0 && adjIdx < pages.length) ? pages[adjIdx] : null;
  }

  /** Cached visual ordering for a scope root. Line membership/order and
   *  per-segment rects are stable within one layout generation; scroll,
   *  resize, or text-layer render bumps _layoutGeneration. Exact positions
   *  are re-read live by callers. */
  _visuallyOrderedTextNodes(root) {
    const cached = this._visualOrderCache;
    if (cached.root === root
      && cached.layoutGeneration === this._layoutGeneration
      && cached.ordered) {
      return { ordered: cached.ordered, lines: cached.lines };
    }
    const result = this._buildVisuallyOrderedTextNodes(root);
    this._visualOrderCache = {
      root, layoutGeneration: this._layoutGeneration,
      ordered: result.ordered, lines: result.lines
    };
    return result;
  }

  /** Build visual ordering and grouped lines for a scope root. */
  _buildVisuallyOrderedTextNodes(root) {
    // Phase 1 — coarse collect: one cheap getBoundingClientRect per node
    // (~27× cheaper than getClientRects); only viewport-band nodes pay the
    // Phase 2 refine.
    const range = document.createRange();
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = [];
    let domIndex = 0;
    while (walker.nextNode()) {
      const textNode = walker.currentNode;
      if (!this._isNavigableTextNode(textNode)) continue;
      const groupRoot = this._isPdfMode() ? null : this._lineGroupingRoot(textNode, root);
      range.selectNodeContents(textNode);
      const boundingRect = range.getBoundingClientRect();
      if (!boundingRect.width || !boundingRect.height) continue;
      nodes.push({
        node: textNode, rect: cloneRect(boundingRect),
        startOffset: 0, endOffset: textNode.length, domIndex, groupRoot
      });
      domIndex++;
    }
    if (!nodes.length) return { ordered: [], lines: [] };

    // Phase 2 — refine: replace coarse segments overlapping the viewport
    // window with real per-fragment segments, before banding so on-screen
    // bands stay tight.
    const margin = this._viewportHeight;
    const viewport = this._viewportRect();
    const vpTop = viewport.top - margin, vpBottom = viewport.bottom + margin;
    for (let i = 0; i < nodes.length; i++) {
      const seg = nodes[i];
      if (seg.rect.bottom < vpTop || seg.rect.top > vpBottom) continue;
      const fragments = this._splitNodeFragments(seg.node, seg.domIndex, seg.groupRoot);
      if (!fragments.length) continue;
      nodes.splice(i, 1, ...fragments);
      i += fragments.length - 1;
    }

    // Cluster into vertical line-bands before sorting so inline boxes that
    // share a visual line but differ in height (e.g. taller <code> spans)
    // sort together by X.
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

  /** Split a text node into per-visual-line segments via getClientRects +
   *  binary search. Returns [{ node, rect, startOffset, endOffset, domIndex,
   *  groupRoot }]. */
  _splitNodeFragments(textNode, domIndex, groupRoot) {
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
      const lineRect = lineRects[li];
      const lineMid = lineRect.top + lineRect.height / 2;
      if (li < lineRects.length - 1) {
        // First offset on the next line: chars past the midpoint between
        // line centers belong to the next line.
        const nextLineRect = lineRects[li + 1];
        const nextLineMid = nextLineRect.top + nextLineRect.height / 2;
        let lo = segStart, hi = textNode.length;
        while (lo < hi) {
          const mid = (lo + hi) >>> 1;
          const rect = this._rangeRectAt(textNode, mid);
          if (!rect) { lo = mid + 1; continue; }
          const charMid = rect.top + rect.height / 2;
          // Is this char on the next line or beyond?
          // Use the midpoint between current and next line centers as the
          // decision boundary. Chars past this boundary belong to the next
          // line or further; chars at or before it belong to the current line.
          const boundary = (lineMid + nextLineMid) / 2;
          if (charMid > boundary) {
            hi = mid;
          } else {
            lo = mid + 1;
          }
        }
        if (lo > segStart) {
          segments.push({
            node: textNode, rect: cloneRect(lineRect),
            startOffset: segStart, endOffset: lo, domIndex, groupRoot
          });
        }
        segStart = lo;
      } else {
        // Last line: rest of the text
        if (textNode.length > segStart) {
          segments.push({
            node: textNode, rect: cloneRect(lineRect),
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
      const bounds = this._lineBounds(lines[i]);
      if (bounds && caretX >= bounds.left && caretX <= bounds.right) return i;
    }
    return bestIdx;
  }

  _pickPositionOnLine(line, goalX) {
    const lineRect = line[0]?.rect;
    if (!lineRect) return null;
    let bestRange = null, bestDist = Infinity;        // visible chars
    let bestWsRange = null, bestWsDist = Infinity;    // whitespace (fallback)
    for (const entry of line) {
      const start = entry.startOffset ?? 0;
      const end = entry.endOffset ?? entry.node.length;
      for (let off = start; off < end; off++) {
        const range = document.createRange();
        range.setStart(entry.node, off); range.setEnd(entry.node, off + 1);
        const rect = range.getBoundingClientRect();
        if (!rect.width || !rect.height) continue;
        if (!this._isSameLine(rect, lineRect)) continue;
        const distance = Math.abs(rect.left + rect.width / 2 - goalX);
        // Prefer a visible character so the caret doesn't land on invisible
        // indentation whitespace.
        if (/\s/.test(entry.node.textContent[off])) {
          if (distance < bestWsDist) { bestWsDist = distance; bestWsRange = this._collapsedRange(entry.node, off); }
        } else if (distance < bestDist) {
          bestDist = distance;
          bestRange = this._collapsedRange(entry.node, off);
        }
      }
    }
    return bestRange || bestWsRange;
  }

  /** Find the segment containing the caret offset in the visual ordering.
   *  Returns { ordered, idx } or null. Shared by char/word movement. */
  _findCaretSegment(focus, focusOff, scopeRoot) {
    const { ordered } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    for (let i = 0; i < ordered.length; i++) {
      const entry = ordered[i];
      if (entry.node === focus && focusOff >= entry.startOffset && focusOff <= entry.endOffset) {
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
    const candidate = this._lineScan(currentLineIndex, isForward, lines, true);
    if (candidate >= 0) return candidate;
    // Caret's column is exhausted — fall back to nearest line in reading
    // order so a two-column page doesn't dead-end.
    return this._lineScan(currentLineIndex, isForward, lines, false);
  }

  _lineScan(currentLineIndex, isForward, lines, checkColumn) {
    const currentBounds = this._lineBounds(lines[currentLineIndex]);
    const curRect = lines[currentLineIndex][0].rect;
    const start = isForward ? currentLineIndex + 1 : currentLineIndex - 1;
    const end = isForward ? lines.length : -1;
    for (let i = start; isForward ? i < end : i > end; i += isForward ? 1 : -1) {
      const targetRect = lines[i][0].rect;
      if (this._isSameLine(targetRect, curRect)) continue;
      if (checkColumn) {
        const targetBounds = this._lineBounds(lines[i]);
        if (currentBounds && targetBounds
          && (currentBounds.right < targetBounds.left
            || targetBounds.right < currentBounds.left)) continue;
      }
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

    // Loop: skip consecutive empty/whitespace-only lines.
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

    // Use the actual focus when it has a usable rect — whitespace-only
    // nodes are real line positions; resolving off them lands on a neighbor
    // line and makes moves look stuck.
    let lineNode = sel.focusNode, lineOffset = sel.focusOffset;
    let caretRect = this._lineMoveRect(lineNode, lineOffset);
    if (!caretRect?.height) {
      const resolved = this._resolveCursorPosition(sel.focusNode, sel.focusOffset, fwd);
      lineNode = resolved.node; lineOffset = resolved.offset;
      const startRange = this._collapsedRange(lineNode, lineOffset);
      caretRect = this._lineMoveRect(lineNode, lineOffset) || this._charRect(startRange);
    }
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
    let forceGapJump = false;

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
        // Caret rect matches no visual line — use closest by Y.
        const midY = caretRect.top + caretRect.height / 2;
        let best = -1, bestD = Infinity;
        for (let i = 0; i < lines.length; i++) {
          const distance = Math.abs(lines[i][0].rect.top + lines[i][0].rect.height / 2 - midY);
          if (distance < bestD) { bestD = distance; best = i; }
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
          const viewport = this._viewportRect();
          const targetInViewport = targetLineRect.top < viewport.bottom && targetLineBottom > viewport.top;
          // A cross-column target (reading-order fallback) can be off screen
          // at any vertical gap — step toward it instead of jumping.
          const currentBounds = this._lineBounds(lines[currentLineIndex]);
          const targetBounds = this._lineBounds(lines[targetLineIndex]);
          const crossColumnOffscreen = !targetInViewport
            && !!currentBounds && !!targetBounds
            && (currentBounds.right < targetBounds.left
              || targetBounds.right < currentBounds.left);
          // Step 200px over a large gap instead of jumping, but only while
          // the caret's own line stays on screen; past that, jump so the
          // cursor stays visible.
          const anchorVisibleAfterStep = fwd
            ? currentLineRect.top - this._scrollPx >= viewport.top
            : currentLineRect.bottom + this._scrollPx <= viewport.bottom;
          if ((lineGap > this._scrollPx || crossColumnOffscreen) && !targetInViewport) {
            if (this._canScroll(fwd) && anchorVisibleAfterStep) {
              // Suppress caret relocation so stepping doesn't bounce back.
              this._suppressScrollRelocate = true;
              this._scrollBy(fwd ? this._scrollPx : -this._scrollPx);
              this._invalidateLayoutCaches();
              // One step per press; no retry/cache — presses continue stepping.
              return { range: null, scrolled: true, stop: true };
            }
            // Anchor would leave the viewport, or at scroll boundary — jump.
            forceGapJump = true;
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
        const bounds = this._lineBounds(lines[j]);
        if (currBounds && bounds && (currBounds.right < bounds.left || bounds.right < currBounds.left)) continue;
        mergedLine.unshift(...lines[j]);
      }
      for (let j = targetLineIndex + 1; j < lines.length; j++) {
        if (!this._isSameLine(lines[j][0].rect, targetLineRect)) break;
        const bounds = this._lineBounds(lines[j]);
        if (currBounds && bounds && (currBounds.right < bounds.left || bounds.right < currBounds.left)) continue;
        mergedLine.push(...lines[j]);
      }
      return { range: this._pickPositionOnLine(mergedLine, goalX), scrolled: false, gapJump: forceGapJump };
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
          const viewport = this._viewportRect();
          const pageNearEdge = fwd
            ? pageRect.top < viewport.top + this._scrollPx
            : pageRect.bottom > viewport.bottom - this._scrollPx;
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
   * Scroll this._scrollPx toward non-text content, then probe for a text
   * line that entered the near viewport. Returns a Range or null.
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

    // Capture mark anchor before any collapse loses it — _markAnchor is the
    // source of truth; _markAnchorPoint clamps a stale offset.
    const markPoint = this.markActive ? this._markAnchorPoint() : null;
    const markAnchorNode = markPoint ? markPoint.node : (this.markActive ? sel.anchorNode : null);
    const markAnchorOff = markPoint ? markPoint.offset : (this.markActive ? sel.anchorOffset : null);

    // Restore the real cursor (pre-extension) so movement starts from the
    // actual caret.
    if (this._savedFocus && this.markActive && sel.rangeCount) {
      sel.collapse(this._savedFocus.node, this._savedFocus.offset);
    }

    this._hitBoundary = false;
    if (this._isAtVisibleBoundary(direction)) {
      this._hitBoundary = true;
      return false;
    }

    // Pre-snap whitespace focus to visible text. Skip for line/
    // lineboundary: they use the visual-line model and a backward snap here
    // would cancel a forward line move.
    const skipPreSnap = granularity === "line" || granularity === "lineboundary";
    if (!skipPreSnap) {
      const { node: snapNode, offset: snapOff } =
        this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
      if (snapNode !== sel.focusNode || snapOff !== sel.focusOffset) {
        if (this.markActive) {
          sel.setBaseAndExtent(markAnchorNode, markAnchorOff, snapNode, snapOff);
        } else {
          sel.collapse(snapNode, snapOff);
        }
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
      // After expandSelection, _moveWordVisual lands at the start of the
      // next word — extend past it so the word is included.
      if (moved && this._savedCaret && granularity === "word") {
        let node = sel.focusNode, offset = sel.focusOffset;
        if (node.nodeType === Node.TEXT_NODE) {
          if (fwd) {
            while (offset < node.textContent.length && this._isWordChar(node.textContent[offset])) offset++;
          } else {
            while (offset > 0 && this._isWordChar(node.textContent[offset - 1])) offset--;
          }
          if (offset !== sel.focusOffset) sel.collapse(node, offset);
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
        if (result.stop) {
          // One-shot step (e.g. over an image): no retry, no cache. Treated
          // as a move so the boundary wrapper doesn't misread pagination.
          this._lineMoveTargetIndex = null;
          this._lineMoveGoalX = null;
          return true;
        }
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

      let lineRange = result.range;
      // _moveLine may stay on the caret's own row (same-Y cluster); if so,
      // probe one line over.
      const movedRect = lineRange && this._rangeRectAt(lineRange.startContainer, lineRange.startOffset);
      if (lineRange && movedRect && preRect && this._isSameLine(movedRect, preRect)) {
        const probe = this._probeLineFromCaret(fwd, preRect);
        if (probe) lineRange = probe;
      }
      if (!lineRange) {
        const atVisibleBoundary = this._isAtVisibleBoundary(direction);
        const atViewportEdge = fwd ? this.isAtBottom() : this.isAtTop();
        this._hitBoundary = atVisibleBoundary || atViewportEdge;
        return false;
      }

      sel.removeAllRanges();
      sel.addRange(lineRange);

      // If caret was off-screen before move, reveal it. A gap jump needs a
      // full reveal — the bounded scroll would leave the target off screen.
      const vpNow = this._viewportRect();
      const preBottom = preRect ? (preRect.bottom ?? (preRect.top + preRect.height)) : null;
      const preOnscreen = preRect && preBottom >= vpNow.top && preRect.top <= vpNow.bottom;
      if (!preOnscreen) {
        this._savedFocus = this.markActive && markAnchorNode != null
          ? this._applyMarkSelection(sel, markAnchorNode, markAnchorOff)
          : { node: sel.focusNode, offset: sel.focusOffset };
        if (result.gapJump) this._scrollToSelection();
        else this._scrollToSelectionLineBounded();
        this._updateCursor();
        return true;
      }

      return this._finishMove(sel, true, markAnchorNode, markAnchorOff);
    }

    // Lineboundary: sel.modify is unreliable in xwidget (goes to text node
    // boundary); use the visual line model.
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
            const range = this._rangeRectAt(seg.node, off);
            if (range) { targetRange = this._collapsedRange(seg.node, off); break; }
          }
        }
      } else {
        // Beginning of line: scan forwards from first segment's start
        for (let si = 0; si < line.length && !targetRange; si++) {
          const seg = line[si];
          for (let off = seg.startOffset; off < seg.endOffset; off++) {
            const range = this._rangeRectAt(seg.node, off);
            if (range) { targetRange = this._collapsedRange(seg.node, off); break; }
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
      if (this.markActive) sel.setBaseAndExtent(markAnchorNode, markAnchorOff, startNode, startOff);
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
   *  extend (end-of-line) shifts the endpoint +1 so the char at the cursor
   *  is included; _savedFocus stores the real (pre-extension) offset. */
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
    // No more text in DOM — but in multi-page PDF, adjacent pages may have
    // unloaded text layers; not a true boundary.
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

  /** Skip past sentence-ending punctuation and whitespace in the given
   *  direction. Returns an offset inside the adjacent sentence. */
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
    this._markAnchor = { node: sp.node, offset: sp.offset };
    this._savedFocus = { node: ep.node, offset: ep.offset };

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
    this._markAnchor = { node: startNode, offset: startOff };
    this._savedFocus = { node: endNode, offset: endOff };
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
    const sel = this._ensureSelection(true);
    this.markActive = true;
    if (sel?.rangeCount) {
      this._markAnchor = { node: sel.focusNode, offset: sel.focusOffset };
      // Keep movement start in sync: a click moved the DOM selection without
      // updating state — a stale _savedFocus would build the region from the
      // pre-click position.
      this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
    } else {
      this._markAnchor = null;
    }
    this._savedCaret = null;
    this._updateCursor();
    return true;
  }

  deactivateMark() {
    this.markActive = false;
    this._markAnchor = null;
    const sel = window.getSelection();
    const saved = this._savedCaret || this._savedFocus;
    this._savedCaret = null;
    if (saved?.node && this._root.contains(saved.node)) {
      sel.collapse(saved.node, saved.offset);
    } else if (sel?.rangeCount) {
      sel.collapse(sel.focusNode, sel.focusOffset);
      this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
    }
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
        this._markAnchor = { node, offset: start };
        this._savedFocus = { node, offset: end };
      } else {
        sel.modify('move', 'forward', 'word');
        sel.modify('extend', 'backward', 'word');
        this._markAnchor = { node: sel.anchorNode, offset: sel.anchorOffset };
        this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
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
      const viewport = this._viewportRect();
      const y = isForward ? viewport.top + 20 : viewport.bottom - 20;
      const el = document.elementFromPoint((viewport.left + viewport.right) / 2, y);
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
    const page = this._currentPage();
    if (page) this._jumpToEdge(true, page);
  }

  endOfPage() {
    const page = this._currentPage();
    if (page) this._jumpToEdge(false, page);
  }

  /** Simulate a mouse click at the current caret position. */
  clickAtCaret() {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return;
    const range = sel.getRangeAt(0).cloneRange();
    range.collapse(true);
    const rect = range.getBoundingClientRect();
    if (!rect?.height) return;
    const x = rect.left + rect.width / 2;
    const y = rect.top + rect.height / 2;
    const el = document.elementFromPoint(x, y);
    if (!el) return;
    el.dispatchEvent(new MouseEvent("click",
      { bubbles: true, cancelable: true, clientX: x, clientY: y }));
  }

  caretInfo() {
    const sel = window.getSelection();
    const saved = this._savedCaret || this._savedFocus || { node: sel.focusNode, offset: sel.focusOffset };
    // Collapsed range at the saved caret
    let rc = this._collapsedRange(saved.node, saved.offset).getBoundingClientRect();
    if (!rc || (!rc.width && !rc.height))
      rc = this._cursorRectAt(saved.node, saved.offset);
    const left = rc?.left ?? 0, bottom = rc?.bottom ?? 0;
    let text = "";
    if (!sel.isCollapsed) {
      text = sel.toString();
      this.deactivateMark();
    } else {
      const node = sel.focusNode, offset = sel.focusOffset;
      if (node?.nodeType === 3) {
        const nodeText = node.textContent;
        let wordStart = offset;
        while (wordStart > 0 && this._isWordChar(nodeText[wordStart - 1])) wordStart--;
        const match = nodeText.slice(wordStart).match(new RegExp('^' + WORD_CHAR_RE.source + '+', 'u'));
        if (match) text = match[0];
      }
    }
    return JSON.stringify({ text, left, bottom });
  }

  /** Remove all event listeners and the cursor overlay. */
  destroy() {
    document.removeEventListener("selectionchange", this._onSelectionChange);
    document.removeEventListener("keydown", this._onKeyDown);
    document.removeEventListener("mousedown", this._onMouseDown);
    if (this.scrollContainer && this._onPdfScroll) {
      this.scrollContainer.removeEventListener("scroll", this._onPdfScroll);
    } else {
      window.removeEventListener("scroll", this._onScroll);
    }
    window.removeEventListener("resize", this._onResize);
    this._cursorEl?.remove();
    this._cursorEl = null;
    this._visualOrderCache = { root: null, layoutGeneration: -1, ordered: null, lines: null };
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
