/** CaretEmacs – Emacs-like caret navigation for WebKit. */

const CURSOR_ID = "__caret-emacs-cursor";
const STYLE_ID = "__caret-emacs-style";
const DEBUG_VERSION = "2026-03-19e";

const CURSOR_CSS = `
#${CURSOR_ID}{
  position:absolute;pointer-events:none;z-index:2147483647;
  background:transparent;display:none;box-sizing:border-box;
  border:1px solid Highlight;opacity:.95;border-radius:1px;
  box-shadow:0 0 0 1px Canvas,0 0 0 2px CanvasText;
}`.trim();

class CaretEmacs {
  constructor(el = document, opts = {}) {
    this.el = el;
    this.markActive = false;
    this._debug = false;
    this._debugLog = [];
    this._onSelectionChange = this._updateCursor.bind(this);
    this.scrollContainer = opts.scrollContainer || null;
    this._cursorEl = null;
    this._scrollRafPending = false;
    this._lastScrollTop = 0;
    this._logDebug("init", { version: DEBUG_VERSION, hasScrollContainer: Boolean(this.scrollContainer) });

    const init = () => {
      this._initCursor();
      document.addEventListener("selectionchange", this._onSelectionChange);
      document.addEventListener("keydown", (e) => {
        if (e.ctrlKey && e.key === 'g' && !e.shiftKey && !e.altKey && !e.metaKey) {
          this.deactivateMark();
        }
      });
      if (this.scrollContainer) {
        this._initPdfScroll();
      } else {
        this._ensureSelection();
        this._updateCursor();
        window.addEventListener('scroll', () => {
          this._onUserScroll();
        }, { passive: true });
        window.addEventListener('resize', () => {
          requestAnimationFrame(() => this._updateCursor());
        }, { passive: true });
      }
    };
    document.body ? init() : document.addEventListener("DOMContentLoaded", init, { once: true });
  }

  get _root() { return this.el === document ? document.body : this.el; }

  /* ── debug helpers ─────────────────────────────────────────── */
  dumpDebug() {
    return this._debugLog.map((entry) => JSON.stringify(entry)).join("\n");
  }

  _logDebug(event, data) {
    if (!this._debug) return;
    this._debugLog.push({ event, ...data });
  }

  enableDebug(enabled = true) {
    this._debug = Boolean(enabled);
    return this._debug;
  }

  clearDebug() {
    this._debugLog = [];
  }

  /* ── scroll / viewport abstraction ───────────────────────── */
  _isContained(node) {
    return node && this._root.contains(node);
  }

  _pageScopeRoot(node) {
    const el = node.nodeType === Node.ELEMENT_NODE ? node : node.parentElement;
    return el?.closest('.page[data-page-number]')
      || el?.closest('.textLayer')
      || this._currentPage()
      || this._root;
  }

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
          placeCaret();
        };
        bus.on('textlayerrendered', onRendered);
      };
      this.scrollContainer.addEventListener('scroll', () => {
        ensureCaret();
        this._onUserScroll();
      }, { passive: true });
      setTimeout(ensureCaret, 300);
    };
    poll();
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

  /* ── tree-walker helpers ─────────────────────────────────── */

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
    while (textNode && !textNode.textContent.trim()) textNode = fwd ? tw.nextNode() : tw.previousNode();
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

  /** Create a collapsed range at the given node and offset. */
  _collapsedRange(node, offset) {
    const range = document.createRange();
    const max = node.nodeType === Node.TEXT_NODE ? node.length : node.childNodes.length;
    range.setStart(node, Math.min(Math.max(0, offset), max));
    range.collapse(true);
    return range;
  }

  /** Make a 1-char range at (node, off) and return its rect, or null. */
  _rangeRectAt(node, off) {
    if (node.nodeType !== Node.TEXT_NODE || !node.length) return null;
    const range = document.createRange();
    const clampedOff = Math.min(off, node.length - 1);
    range.setStart(node, clampedOff);
    range.setEnd(node, clampedOff + 1);
    const rect = range.getClientRects()[0] || range.getBoundingClientRect();
    return (rect?.height && rect?.width) ? rect : null;
  }

  /* ── selection helpers ──────────────────────────────────────── */

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
    const r = this._collapsedRange(sel.focusNode, sel.focusOffset);
    let rect = r.getBoundingClientRect();
    if (!rect.height)
      rect = this._rangeRectAt(sel.focusNode, sel.focusOffset);
    if (!rect) {
      const { node, offset } = this._resolveCursorPosition(
        sel.focusNode, sel.focusOffset, true);
      if (node.nodeType === Node.TEXT_NODE)
        rect = this._rangeRectAt(node, offset);
    }
    const vp = this._viewportRect();
    if (rect && rect.bottom >= vp.top && rect.top <= vp.bottom) return;
    const cx = (vp.left + vp.right) / 2;
    const cy = vp.top + 1;
    const resolved = this._probeTextAt(cx, cy);
    if (resolved) {
      sel.removeAllRanges();
      sel.addRange(resolved);
    }
  }

  _onUserScroll() {
    if (this._scrollRafPending) return;
    this._scrollRafPending = true;
    requestAnimationFrame(() => {
      this._scrollRafPending = false;
      const scrollTop = this._scrollTop;
      const fwd = scrollTop > this._lastScrollTop;
      this._lastScrollTop = scrollTop;

      const sel = window.getSelection();
      if (!sel?.rangeCount) {
        this._ensureSelection();
        this._updateCursor();
        return;
      }

      const vp = this._viewportRect();
      const caretRect = this._isContained(sel.focusNode)
        ? this._rangeRectAt(sel.focusNode, sel.focusOffset) : null;
      if (caretRect && caretRect.bottom >= vp.top && caretRect.top <= vp.bottom) return;

      // Use caret's current X (still valid even if off-screen), or viewport center
      const cx = caretRect
        ? caretRect.left + caretRect.width / 2
        : (vp.left + vp.right) / 2;
      const cy = fwd ? vp.top + 20 : vp.bottom - 20;

      const resolved = this._probeTextAt(cx, cy);
      if (resolved) {
        sel.removeAllRanges();
        sel.addRange(resolved);
        this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
      }
      this._updateCursor();
    });
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

  /** Scroll the viewport to keep the selection focus visible. Returns true if scrolled. */
  _scrollToSelection() {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return false;

    // Use _rangeRectAt for a proper rect with height, fallback to collapsed range
    let rect = this._rangeRectAt(sel.focusNode, sel.focusOffset);
    if (!rect) {
      rect = this._collapsedRange(sel.focusNode, sel.focusOffset).getBoundingClientRect();
    }
    if (!rect || !rect.height) return false;

    const { top, bottom } = rect;
    const vp = this._viewportRect();
    if (bottom > vp.bottom) {
      if (Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight) return false;
      // Scroll to place cursor at 1/3 from bottom of viewport
      const vpHeight = this._viewportHeight;
      const delta = bottom - (vp.top + vpHeight * 1 / 3);
      if (delta <= 0) return false;
      this._scrollBy(delta);
      return true;
    } else if (top < vp.top) {
      if (Math.floor(this._scrollTop) <= 0) return false;
      // Scroll to place cursor at 2/3 from top of viewport
      const vpHeight = this._viewportHeight;
      const delta = top - (vp.top + vpHeight * 2 / 3);
      if (delta >= 0) return false;
      this._scrollBy(delta);
      return true;
    }
    return false;
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

  /** Set selection focus; extend from anchor if mark is active. */
  _setFocus(sel, node, off, anchorNode, anchorOff) {
    if (anchorNode != null) sel.setBaseAndExtent(anchorNode, anchorOff, node, off);
    else sel.collapse(node, off);
  }

  /* ── cursor overlay ─────────────────────────────────────────── */

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

  /** Resolve element/whitespace focus to a visible text position. */
  _resolveCursorPosition(node, offset, preferFwd = false) {
    if (node.nodeType === Node.ELEMENT_NODE) {
      const child = offset < node.childNodes.length
        ? node.childNodes[offset]
        : node.lastChild;

      // Check direct child and its firstChild before walking
      if (preferFwd) {
        if (child?.nodeType === Node.TEXT_NODE && child.textContent.trim())
          return { node: child, offset: 0 };
        if (child?.nodeType === Node.ELEMENT_NODE) {
          const inner = child.firstChild;
          if (inner?.nodeType === Node.TEXT_NODE && inner.textContent.trim())
            return { node: inner, offset: 0 };
        }
      }

      const start = child || node;
      const t = this._walkToVisible(start, true) || this._walkToVisible(start, false);
      if (t) return { node: t, offset: 0 };
    }
    if (node.nodeType === Node.TEXT_NODE && !node.textContent.trim()) {
      const prev = this._walkToVisible(node, false);
      if (prev) return { node: prev, offset: prev.length };
      const next = this._walkToVisible(node, true);
      if (next) return { node: next, offset: 0 };
    }
    return { node, offset };
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
      const fontSize = parseFloat(getComputedStyle(parent).fontSize);
      if (fontSize > 0 && cursorHeight > fontSize) {
        cursorTop += (cursorHeight - fontSize) / 2;
        cursorHeight = fontSize;
      }
    }
    Object.assign(el.style, {
      display: "block",
      left: `${cursorLeft}px`,
      top: `${cursorTop}px`,
      width: `${cw}px`,
      height: `${cursorHeight}px`,
    });

    // Save current focus as last-rendered position
    this._lastRenderedPos = { node: sel.focusNode, offset: sel.focusOffset };
  }

  /* ── movement ───────────────────────────────────────────────── */

  /** Snap the selection focus onto a visible text node. */
  _snapToText(sel, fwd) {
    const focus = sel.focusNode;
    if (focus.nodeType === Node.TEXT_NODE && focus.textContent.trim()) return;

    const pastEnd = focus.nodeType === Node.ELEMENT_NODE &&
      fwd && sel.focusOffset >= focus.childNodes.length;
    const lookFwdFirst = pastEnd ? false
      : focus.nodeType === Node.ELEMENT_NODE ? fwd : !fwd;

    const textNode = this._walkToVisible(focus, lookFwdFirst)
      || this._walkToVisible(focus, !lookFwdFirst);
    if (textNode) sel.collapse(textNode, fwd ? textNode.length : 0);
  }

  /** Visual character movement for PDF text layers (bypass DOM order). */
  _moveCharVisual(sel, fwd) {
    const focus = sel.focusNode;
    const focusOff = sel.focusOffset;
    if (focus.nodeType !== Node.TEXT_NODE) return false;

    if (fwd) {
      if (focusOff + 1 < focus.length) {
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
      sel.collapse(prev, Math.max(0, prev.length - 1));
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
      while (off < node.textContent.length && /\w/.test(node.textContent[off])) off++;
      // Skip non-word chars, crossing node boundaries as needed
      while (true) {
        while (off < node.textContent.length && !/\w/.test(node.textContent[off])) off++;
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
        while (off >= 0 && !/\w/.test(node.textContent[off])) off--;
        if (off >= 0) break;
        idx--;
        if (idx < 0) return false;
        node = ordered[idx].node;
        off = node.textContent.length - 1;
      }
      // Skip word chars backward to find word start
      while (off > 0 && /\w/.test(node.textContent[off - 1])) off--;
      sel.collapse(node, off);
      return true;
    }
  }

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

    // Common epilogue: restore mark, save caret, scroll & redraw
    const finish = (moved) => {
      if (this.markActive) {
        sel.setBaseAndExtent(markAnchorNode, markAnchorOff, sel.focusNode, sel.focusOffset);
      }
      if (moved) {
        this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
        this._scrollToSelection();
        this._updateCursor();
      }
      return moved;
    };

    // Character / word: visual ordering (PDF text layers have DOM ≠ visual order)
    if (granularity === "character" || granularity === "word") {
      const moved = granularity === "character"
        ? this._moveCharVisual(sel, fwd)
        : this._moveWordVisual(sel, fwd);
      return finish(moved);
    }

    // Line: caretRangeFromPoint for visual line movement
    if (granularity === "line" && this.scrollContainer) {
      const lineRange = this._moveLine(fwd);
      if (!lineRange) { this._hitBoundary = true; return false; }
      sel.removeAllRanges();
      sel.addRange(lineRange);
      return finish(true);
    }

    // Default (sentence, paragraph, etc.): sel.modify with guards
    let moved = this._stepModify(sel, direction, granularity, fwd);
    this._snapToText(sel, fwd);
    if (sel.focusNode === startNode && sel.focusOffset === startOff) { moved = false; }

    if (moved && this._movedWrongWay(startNode, startOff, sel.focusNode, sel.focusOffset, fwd)) {
      this._setFocus(sel, startNode, startOff,
        this.markActive ? sel.anchorNode : null,
        this.markActive ? sel.anchorOffset : null);
      moved = false;
    }

    // Fallback: void elements that sel.modify cannot cross
    if (!moved && this._fallbackToAdjacentText(sel, fwd)) return true;

    return finish(moved);
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

  /** Jump to the next/prev visible text node when sel.modify fails. */
  _fallbackToAdjacentText(sel, fwd) {
    const startNode = sel.focusNode.nodeType === Node.TEXT_NODE
      ? sel.focusNode
      : (sel.focusNode.childNodes[sel.focusOffset] || sel.focusNode);
    const textNode = this._walkToVisible(startNode, fwd);
    if (!textNode) return false;
    this._setFocus(sel, textNode, fwd ? 0 : textNode.length,
      this.markActive ? sel.anchorNode : null,
      this.markActive ? sel.anchorOffset : null);
    this._savedFocus = { node: sel.focusNode, offset: sel.focusOffset };
    this._scrollToSelection();
    this._updateCursor();
    return true;
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

  /** Get a rect with real dimensions at a range's start position. */
  _charRect(range) {
    const rect = this._rangeRectAt(range.startContainer, range.startOffset);
    if (rect) return rect;
    const fallbackRect = range.getBoundingClientRect();
    return fallbackRect?.height ? fallbackRect : null;
  }

  /** Resolve a range from caretRangeFromPoint to a visible text node, or null. */
  _rangeToText(range) {
    const { node, offset } = this._resolveCursorPosition(
      range.startContainer, range.startOffset, true);
    if (node.nodeType !== Node.TEXT_NODE || !node.textContent.trim()) return null;
    return this._collapsedRange(node, offset);
  }

  /** DOM-based visual line movement. Returns a collapsed Range on success, or null. */
  _moveLine(fwd) {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return null;
    const r0 = this._collapsedRange(sel.focusNode, sel.focusOffset);
    const rect = this._charRect(r0);
    if (!rect?.height) return null;
    const goalX = rect.left;

    // Phase 1: move within current page
    const page = this._currentPage();
    const scopeRoot = page || this._root;
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    const lines = this._groupIntoLines(ordered);
    if (!lines.length) return null;
    const curLineIdx = this._findCaretLine(lines, rect);
    if (curLineIdx >= 0) {
      const targetIdx = fwd ? curLineIdx + 1 : curLineIdx - 1;
      if (targetIdx >= 0 && targetIdx < lines.length)
        return this._pickPositionOnLine(lines[targetIdx], goalX);
    }

    // Phase 2: cross page boundary (skip pages with no text)
    if (!page) return null;
    let adj = page;
    let skipped = 0;
    while ((adj = this._visuallyAdjacentPage(adj, fwd)) && skipped < 5) {
      const adjOrdered = this._visuallyOrderedTextNodes(adj);
      if (!adjOrdered.length) { skipped++; continue; }
      // Found text — scroll into view if needed, then pick line
      const adjRect = adj.getBoundingClientRect();
      const vp = this._viewportRect();
      if (adjRect.bottom < vp.top || adjRect.top > vp.bottom)
        adj.scrollIntoView({ block: fwd ? 'start' : 'end' });
      const adjLines = this._groupIntoLines(adjOrdered);
      if (!adjLines.length) { skipped++; continue; }
      const targetLine = fwd ? adjLines[0] : adjLines[adjLines.length - 1];
      return this._pickPositionOnLine(targetLine, goalX);
    }
    // Scroll first adjacent page into view to trigger text layer loading
    const firstAdj = this._visuallyAdjacentPage(page, fwd);
    if (firstAdj)
      firstAdj.scrollIntoView({ block: fwd ? 'start' : 'end' });
    return null;
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

  /* ── boundary detection ──────────────────────────────────────── */

  _isAtVisibleBoundary(direction) {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return false;
    const node = sel.focusNode, offset = sel.focusOffset;
    const fwd = direction === "forward";
    if (node.nodeType !== Node.TEXT_NODE) return false;
    if (!node.textContent.trim()) return !this._walkToVisible(node, fwd);
    if (fwd ? offset < node.length - 1 : offset > 0) return false;
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

  _pdfAdjacentLineBlockCheck(currentLine, candidateLine, fwd) {
    const current = this._lineBounds(currentLine);
    const candidate = this._lineBounds(candidateLine);
    if (!current || !candidate) return false;

    const minHeight = Math.min(current.height, candidate.height);

    // Alignment: reject lines at very different X positions
    if (Math.abs(candidate.left - current.left) > minHeight * 1.5) return false;

    // Gap: reject lines with too much vertical space between them
    const upperBounds = fwd ? current : candidate;
    const lowerBounds = fwd ? candidate : current;
    const gap = Math.max(0, lowerBounds.top - upperBounds.bottom);
    return gap <= minHeight * 1.2;
  }

  _expandPdfLineBlock(lines, startIdx, endIdx) {
    let start = startIdx;
    let end = endIdx;

    while (start > 0) {
      if (!this._pdfAdjacentLineBlockCheck(lines[start], lines[start - 1], false)) break;
      start--;
    }

    while (end < lines.length - 1) {
      if (!this._pdfAdjacentLineBlockCheck(lines[end], lines[end + 1], true)) break;
      end++;
    }

    return { start, end };
  }

  _pdfLineContext(sel) {
    const { node, offset } = this._resolveCursorPosition(sel.anchorNode, sel.anchorOffset);
    if (node.nodeType !== Node.TEXT_NODE || !this._isContained(node)) return null;

    const scopeRoot = this._pageScopeRoot(node);
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;

    const lines = this._groupIntoLines(ordered);
    if (!lines.length) return null;

    const caretRange = this._collapsedRange(node, offset);
    const caretRect = this._charRect(caretRange) || caretRange.getBoundingClientRect();
    if (!caretRect?.height) return null;

    const currentLine = this._findCaretLine(lines, caretRect);
    return currentLine < 0 ? null : { lines, currentLine };
  }

  _selectionCoversPdfLines(sel) {
    const range = sel.getRangeAt(0);
    const startNode = range.startContainer;
    const endNode = range.endContainer;
    if (startNode.nodeType !== Node.TEXT_NODE || endNode.nodeType !== Node.TEXT_NODE) return null;

    const scopeRoot = this._pageScopeRoot(startNode);
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;

    const lines = this._groupIntoLines(ordered);
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
    const sentenceEndings = trimmedText.match(/[.!?](?=\s|$)/g) || [];
    if (sentenceEndings.length > 1) return 'paragraph';
    if (/[.!?]\s*$/.test(trimmedText)) return 'sentence';
    return (end > start || /\n/.test(text)) ? 'paragraph' : 'word';
  }

  _pdfTextRangeModel(lines, startIdx, endIdx) {
    const blockLines = lines.slice(startIdx, endIdx + 1).map((line, offset) => ({
      index: startIdx + offset,
      entries: line || []
    }));
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
    while (start < text.length && /[\s\n]/.test(text[start])) start++;

    let end = text.length;
    for (let idx = normalizedCaret; idx < text.length; idx++) {
      if (/[.!?]/.test(text[idx])) {
        end = idx + 1;
        break;
      }
    }
    while (end > start && /\s/.test(text[end - 1])) end--;

    return { start, end };
  }

  _expandPdf(sel, mode) {
    const context = this._pdfLineContext(sel);
    if (!context) {
      return false;
    }

    const { lines, currentLine } = context;
    const selectedRange = this._selectedPdfLineRange(sel, lines);
    const anchorStart = selectedRange ? Math.min(currentLine, selectedRange.start) : currentLine;
    const anchorEnd = selectedRange ? Math.max(currentLine, selectedRange.end) : currentLine;
    const { start, end } = this._expandPdfLineBlock(lines, anchorStart, anchorEnd);

    if (mode === 'sentence') {
      const model = this._pdfTextRangeModel(lines, start, end);
      const caretPoint = this._resolveCursorPosition(sel.anchorNode, sel.anchorOffset);
      const caretOffset = model.offsetFromPoint(caretPoint.node, caretPoint.offset);
      const offsets = this._pdfSentenceOffsets(model.text, caretOffset);

      const sp = model.pointFromOffset(offsets.start);
      const ep = model.pointFromOffset(offsets.end);
      if (!sp?.node || !ep?.node) return false;
      sel.setBaseAndExtent(ep.node, ep.offset, sp.node, sp.offset);
    } else {
      const startBounds = this._lineBounds(lines[start]);
      const endBounds = this._lineBounds(lines[end]);
      if (!startBounds || !endBounds) return false;

      sel.setBaseAndExtent(endBounds.last.node, endBounds.last.node.length, startBounds.first.node, 0);
    }

    this.markActive = true;
    return true;
  }

  _expandTo(sel, granularity) {
    if (this.scrollContainer) {
      if (granularity === 'sentenceboundary' && this._expandPdf(sel, 'sentence')) return;
      if (granularity === 'paragraphboundary' && this._expandPdf(sel, 'paragraph')) return;
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

  /* ── public API (for programmatic / Emacs xwidget dispatch) ── */

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
    } else {
      this._expandTo(sel, 'paragraphboundary');
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

  _visuallyOrderedTextNodes(root) {
    const tw = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = [];
    while (tw.nextNode()) {
      const textNode = tw.currentNode;
      if (!textNode.textContent.trim()) continue;
      const range = document.createRange();
      range.selectNodeContents(textNode);
      const rects = range.getClientRects();
      const clientRect = rects[0] || range.getBoundingClientRect();
      if (!clientRect || !clientRect.height || !clientRect.width) {
        continue;
      }
      const rect = {
        top: clientRect.top,
        left: clientRect.left,
        width: clientRect.width,
        height: clientRect.height
      };
      nodes.push({ node: textNode, rect });
    }
    nodes.sort((a, b) => {
      // Calculate vertical midpoints for more accurate line detection
      const aMid = a.rect.top + a.rect.height / 2;
      const bMid = b.rect.top + b.rect.height / 2;
      const dy = aMid - bMid;

      // Use adaptive tolerance based on the heights of both elements
      // This handles superscripts, subscripts, and varying font sizes
      const tolerance = Math.max(a.rect.height, b.rect.height) / 2;

      if (Math.abs(dy) > tolerance) return dy;
      return a.rect.left - b.rect.left;
    });
    return nodes;
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

  _groupIntoLines(orderedNodes) {
    if (!orderedNodes.length) return [];
    const lines = [];
    let currentLine = [orderedNodes[0]];
    for (let i = 1; i < orderedNodes.length; i++) {
      const entry = orderedNodes[i];
      const firstRect = currentLine[0].rect;
      const entryMid = entry.rect.top + entry.rect.height / 2;
      const repMid = firstRect.top + firstRect.height / 2;
      const tolerance = Math.max(entry.rect.height, firstRect.height) / 2;
      if (Math.abs(entryMid - repMid) <= tolerance) {
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
    const caretMid = caretRect.top + caretRect.height / 2;
    let bestIdx = -1, bestDist = Infinity;
    for (let i = 0; i < lines.length; i++) {
      const firstRect = lines[i][0].rect;
      const lineMid = firstRect.top + firstRect.height / 2;
      const dist = Math.abs(caretMid - lineMid);
      if (dist < bestDist) { bestDist = dist; bestIdx = i; }
    }
    const lineH = lines[bestIdx]?.[0].rect.height || 0;
    return bestDist > lineH * 1.5 ? -1 : bestIdx;
  }

  _pickPositionOnLine(line, goalX) {
    const repRect = line[0].rect;
    const lineMidY = repRect.top + repRect.height / 2;
    // Build a set of nodes on this line for membership check
    const lineNodes = new Set(line.map(e => e.node));
    // Try caretRangeFromPoint for precision
    const probe = document.caretRangeFromPoint(goalX, lineMidY);
    if (probe) {
      const resolved = this._rangeToText(probe);
      if (resolved && this._isContained(resolved.startContainer)
        && lineNodes.has(resolved.startContainer)) {
        const cr = this._charRect(resolved);
        if (cr && Math.abs(cr.top - repRect.top) < repRect.height) return resolved;
      }
    }

    // Fallback: character-level walk
    let bestRange = null, bestDist = Infinity;
    for (const { node, rect } of line) {
      for (let off = 0; off < node.length; off++) {
        const r = document.createRange();
        r.setStart(node, off); r.setEnd(node, off + 1);
        const cr = r.getBoundingClientRect();
        if (!cr.width || !cr.height) continue;
        const d = Math.abs(cr.left + cr.width / 2 - goalX);
        if (d < bestDist) {
          bestDist = d;
          bestRange = this._collapsedRange(node, off);
        }
      }
    }
    return bestRange;
  }

  _jumpToEdge(toStart, root) {
    const sel = this._ensureSelection();
    if (!sel) return;
    const scopeRoot = root || this._root;
    const tw = document.createTreeWalker(scopeRoot, NodeFilter.SHOW_TEXT);
    let bestNode = null, bestTop = null, bestLeft = null;
    while (tw.nextNode()) {
      const textNode = tw.currentNode;
      if (!textNode.textContent.trim()) continue;
      const off = toStart ? 0 : Math.max(0, textNode.length - 1);
      const rect = this._rangeRectAt(textNode, off);
      if (!rect) continue;
      if (bestNode === null) {
        bestNode = textNode; bestTop = rect.top; bestLeft = rect.left;
        continue;
      }
      const dy = rect.top - bestTop;
      const better = toStart
        ? (dy < -2 || (Math.abs(dy) <= 2 && rect.left < bestLeft))
        : (dy > 2 || (Math.abs(dy) <= 2 && rect.left > bestLeft));
      if (better) {
        bestNode = textNode; bestTop = rect.top; bestLeft = rect.left;
      }
    }
    let range;
    if (bestNode) {
      range = this._collapsedRange(bestNode, toStart ? 0 : Math.max(0, bestNode.length - 1));
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

  beginningOfBuffer() { this._jumpToEdge(true); }
  endOfBuffer() { this._jumpToEdge(false); }

  _currentPage() {
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
}

window.CaretEmacs = CaretEmacs;
const viewerContainer = document.getElementById('viewerContainer');
const viewer = document.getElementById('viewer');
if (viewerContainer && viewer) {
  window.__caretEmacs = new CaretEmacs(viewer, { scrollContainer: viewerContainer });
} else {
  window.__caretEmacs = new CaretEmacs(document);
}
