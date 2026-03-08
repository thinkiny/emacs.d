/** CaretEmacs – Emacs-like caret navigation for WebKit. */

const PAGE_OVERLAP = 40;
const CURSOR_ID = "__caret-emacs-cursor";
const STYLE_ID = "__caret-emacs-style";
const DEBUG_VERSION = "2026-03-07a";

const CURSOR_CSS = `
#${CURSOR_ID}{
  position:absolute;pointer-events:none;z-index:2147483647;
  background:highlight;display:none;box-sizing:border-box;
  animation:__caret-emacs-blink 1s step-end infinite
}
@keyframes __caret-emacs-blink{50%{opacity:0}}
`.trim();

class CaretEmacs {
  constructor(el = document, opts = {}) {
    this.el = el;
    this.markActive = false;
    this._debug = false
    this._debugLog = [];
    this._onSelectionChange = this._updateCursor.bind(this);
    this.scrollContainer = opts.scrollContainer || null;
    this._cursorEl = null;
    this._scrollRafPending = false;
    this._lastScrollTop = 0;
    this._logDebug("init", {
      version: DEBUG_VERSION,
      hasScrollContainer: Boolean(this.scrollContainer),
    });

    const init = () => {
      this._initCursor();
      document.addEventListener("selectionchange", this._onSelectionChange);
      if (this.scrollContainer) {
        this._initPdfScroll();
      } else {
        this._ensureSelection();
        this._updateCursor();
        window.addEventListener('scroll', () => {
          this._updateCursor();
          this._onUserScroll();
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

  /* ── scroll / viewport abstraction ───────────────────────── */
  _isContained(node) {
    return node && this._root.contains(node);
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
        const pn = window.PDFViewerApplication?.page;
        const tl = document.querySelector(`.page[data-page-number="${pn}"] .textLayer`);
        if (tl?.children.length) { placeCaret(); return; }
        const onRendered = (e) => {
          if (e.pageNumber !== pn) return;
          bus.off('textlayerrendered', onRendered);
          placeCaret();
        };
        bus.on('textlayerrendered', onRendered);
      };
      this.scrollContainer.addEventListener('scroll', () => {
        ensureCaret();
        this._updateCursor();
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
      const el = range.startContainer.nodeType === Node.TEXT_NODE
        ? range.startContainer.parentElement : range.startContainer;
      if (!el?.closest('.textLayer')) return null;
    }
    return this._resolveToText(range);
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
    let t = fwd ? tw.nextNode() : tw.previousNode();
    while (t && !t.textContent.trim()) t = fwd ? tw.nextNode() : tw.previousNode();
    return t;
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
    const r = document.createRange();
    r.setStart(node, offset);
    r.collapse(true);
    return r;
  }

  /** Make a 1-char range at (node, off) and return its rect, or null. */
  _rangeRectAt(node, off) {
    if (node.nodeType !== Node.TEXT_NODE || !node.length) return null;
    const r = document.createRange();
    const o = Math.min(off, node.length - 1);
    r.setStart(node, o);
    r.setEnd(node, o + 1);
    const rect = r.getClientRects()[0] || r.getBoundingClientRect();
    return (rect?.height && rect?.width) ? rect : null;
  }

  /* ── selection helpers ──────────────────────────────────────── */

  _ensureSelection(skipRelocate = false) {
    const sel = window.getSelection();
    if (sel?.rangeCount > 0) {
      if (!skipRelocate) this._relocateIfOffscreen(sel);
      return sel;
    }
    const c = this._savedCaret;
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
        this._savedCaret = { node: sel.focusNode, offset: sel.focusOffset };
      }
      this._updateCursor();
    });
  }

  _setMark(active) {
    this.markActive = active;
    if (!active) {
      const sel = window.getSelection();
      if (sel?.rangeCount) {
        const fn = sel.focusNode;
        const fo = sel.focusOffset;
        sel.collapse(fn, fo);
        this._savedCaret = { node: fn, offset: fo };
        setTimeout(() => this._restoreCaretIfLost(), 0);
      }
    }
    this._updateCursor();
  }

  _restoreCaretIfLost() {
    const sel = window.getSelection();
    if (sel?.rangeCount) return;
    const c = this._savedCaret;
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
    if (bottom > vp.bottom - PAGE_OVERLAP) {
      if (Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight) return false;
      // Scroll to place cursor at 1/3 from bottom of viewport
      const vpHeight = this._viewportHeight;
      const delta = bottom - (vp.top + vpHeight * 2 / 3);
      if (delta <= 0) return false;
      this._scrollBy(delta);
      return true;
    } else if (top < vp.top + PAGE_OVERLAP) {
      if (Math.floor(this._scrollTop) <= 0) return false;
      // Scroll to place cursor at 1/3 from top of viewport
      const vpHeight = this._viewportHeight;
      const delta = top - (vp.top + vpHeight / 3);
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
  _setFocus(sel, node, off, an, ao) {
    if (an != null) sel.setBaseAndExtent(an, ao, node, off);
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
      const c = this._lastCursorPos;
      if (c?.node && this._root.contains(c.node)) {
        sel.removeAllRanges();
        sel.addRange(this._collapsedRange(c.node, c.offset));
      }
      if (!sel?.rangeCount) return void (el.style.display = "none");
    }
    if (!this._isContained(sel.focusNode)) return void (el.style.display = "none");
    const { node, offset } = this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    const rect = this._cursorRectAt(node, offset);
    if (!rect?.height) return void (el.style.display = "none");
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
    this._lastCursorPos = { node: sel.focusNode, offset: sel.focusOffset };
  }

  /* ── movement ───────────────────────────────────────────────── */

  /** Snap the selection focus onto a visible text node. */
  _snapToText(sel, fwd) {
    const fn = sel.focusNode;
    if (fn.nodeType === Node.TEXT_NODE && fn.textContent.trim()) return;

    const pastEnd = fn.nodeType === Node.ELEMENT_NODE &&
      fwd && sel.focusOffset >= fn.childNodes.length;
    const lookFwdFirst = pastEnd ? false
      : fn.nodeType === Node.ELEMENT_NODE ? fwd : !fwd;

    const t = this._walkToVisible(fn, lookFwdFirst)
      || this._walkToVisible(fn, !lookFwdFirst);
    if (t) sel.collapse(t, fwd ? t.length : 0);
  }

  /** Visual character movement for PDF text layers (bypass DOM order). */
  _moveCharVisual(sel, fwd) {
    const fn = sel.focusNode;
    const fo = sel.focusOffset;
    if (fn.nodeType !== Node.TEXT_NODE) return false;

    if (fwd) {
      if (fo + 1 < fn.length) {
        sel.collapse(fn, fo + 1);
        return true;
      }
      const next = this._visuallyAdjacentTextNode(true);
      if (!next) return false;
      sel.collapse(next, 0);
      return true;
    } else {
      if (fo > 0) {
        sel.collapse(fn, fo - 1);
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
    const fn = sel.focusNode;
    const fo = sel.focusOffset;
    if (fn.nodeType !== Node.TEXT_NODE) return false;

    const el = fn.parentElement;
    const page = el?.closest('.page[data-page-number]');
    const scopeRoot = page || el?.closest('.textLayer') || this._root;
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return false;

    const curIdx = ordered.findIndex(e => e.node === fn);
    if (curIdx === -1) return false;

    if (fwd) {
      let node = fn, off = fo, idx = curIdx;
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
      let node = fn, off = fo, idx = curIdx;
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
    const sel = this._ensureSelection(true); // Skip relocate for precise movements
    if (!sel) return false;

    this._hitBoundary = false;
    if (this._isAtVisibleBoundary(direction)) {
      this._hitBoundary = true;
      return false;
    }

    // Pre-snap: if on whitespace node, snap to visible text first
    const { node: snapN, offset: snapO } =
      this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    if (snapN !== sel.focusNode || snapO !== sel.focusOffset) {
      sel.collapse(snapN, snapO);
    }

    if (!this.markActive && !sel.isCollapsed)
      sel.collapse(sel.focusNode, sel.focusOffset);

    const fwd = direction === "forward";

    const step = () => {
      const { focusNode: fn0, focusOffset: fo0 } = sel;
      sel.modify("move", direction, granularity);
      if (!this._isContained(sel.focusNode)) {
        sel.collapse(fn0, fo0);
        return false;
      }
      if ((sel.focusNode !== fn0 || sel.focusOffset !== fo0) &&
        this._movedWrongWay(fn0, fo0, sel.focusNode, sel.focusOffset, fwd)) {
        sel.collapse(fn0, fo0);
        return false;
      }
      this._unstickCaret(sel, direction, fn0, fo0);
      return sel.focusNode !== fn0 || sel.focusOffset !== fo0;
    };

    const initNode = sel.focusNode;
    const initOff = sel.focusOffset;

    // Save anchor for mark-active; collapse focus for stepping
    const an = this.markActive ? sel.anchorNode : null;
    const ao = this.markActive ? sel.anchorOffset : null;
    if (this.markActive) sel.collapse(sel.focusNode, sel.focusOffset);

    // Use visual ordering for character/word movement
    if (granularity === "character" || granularity === "word") {
      let moved;
      if (granularity === "character") {
        moved = this._moveCharVisual(sel, fwd);
      } else {
        moved = this._moveWordVisual(sel, fwd);
      }
      if (this.markActive)
        sel.setBaseAndExtent(an, ao, sel.focusNode, sel.focusOffset);
      if (moved) {
        this._savedCaret = { node: sel.focusNode, offset: sel.focusOffset };
        this._scrollToSelection(); this._updateCursor();
      }
      return moved;
    }

    if (granularity === "line" && this.scrollContainer) {
      const lineRange = this._moveLine(fwd);
      if (lineRange) {
        if (this.markActive) {
          sel.setBaseAndExtent(an, ao, lineRange.startContainer, lineRange.startOffset);
        } else {
          sel.removeAllRanges();
          sel.addRange(lineRange);
        }
        this._savedCaret = { node: sel.focusNode, offset: sel.focusOffset };
        this._scrollToSelection(); this._updateCursor();
        return true;
      }
      // _moveLine already searched through all adjacent pages.
      this._hitBoundary = true;
      return false;
    }

    let moved = step();
    while (moved && sel.focusNode.nodeType === Node.ELEMENT_NODE) {
      if (!step()) break;
    }
    this._snapToText(sel, fwd);
    if (sel.focusNode === initNode && sel.focusOffset === initOff) moved = false;

    if (this.markActive)
      sel.setBaseAndExtent(an, ao, sel.focusNode, sel.focusOffset);

    // Guard: if step + snapToText left us in the wrong direction, revert.
    if (moved && this._movedWrongWay(initNode, initOff, sel.focusNode, sel.focusOffset, fwd)) {
      this._setFocus(sel, initNode, initOff,
        this.markActive ? sel.anchorNode : null,
        this.markActive ? sel.anchorOffset : null);
      moved = false;
    }

    // Fallback: void elements that sel.modify cannot cross.
    if (!moved && this._fallbackToAdjacentText(sel, fwd)) {
      this._savedCaret = { node: sel.focusNode, offset: sel.focusOffset };
      return true;
    }

    this._savedCaret = { node: sel.focusNode, offset: sel.focusOffset };
    this._scrollToSelection(); this._updateCursor();
    return moved;
  }

  /** Jump to the next/prev visible text node when sel.modify fails. */
  _fallbackToAdjacentText(sel, fwd) {
    const startNode = sel.focusNode.nodeType === Node.TEXT_NODE
      ? sel.focusNode
      : (sel.focusNode.childNodes[sel.focusOffset] || sel.focusNode);
    const t = this._walkToVisible(startNode, fwd);
    if (!t) return false;
    this._setFocus(sel, t, fwd ? 0 : t.length,
      this.markActive ? sel.anchorNode : null,
      this.markActive ? sel.anchorOffset : null);
    this._scrollToSelection(); this._updateCursor();
    return true;
  }

  /** Nudge the caret past elements that sel.modify cannot cross. */
  _unstickCaret(sel, direction, oldNode, oldOff) {
    const fn = sel.focusNode, fwd = direction === "forward";
    // Case 1: inside an empty/void element — hop to parent edge.
    if (fn.nodeType === Node.ELEMENT_NODE && !fn.firstChild) {
      const parent = fn.parentNode;
      if (parent) {
        const idx = Array.from(parent.childNodes).indexOf(fn);
        sel.collapse(parent, fwd ? idx + 1 : idx);
      }
      return;
    }
    if (fn !== oldNode || sel.focusOffset !== oldOff) return; // moved — nothing to fix
    // Case 2: stuck at a text-node edge — walk to the next text node.
    if (fn.nodeType === Node.TEXT_NODE) {
      if (fwd && oldOff < fn.length) return;
      if (!fwd && oldOff > 0) return;
      const t = this._walkToVisible(fn, fwd);
      if (t) sel.collapse(t, fwd ? 0 : t.length);
      return;
    }
    // Case 3: stuck at an element position — step over adjacent empty child.
    if (fn.nodeType === Node.ELEMENT_NODE) {
      const adj = fn.childNodes[fwd ? oldOff : oldOff - 1];
      if (adj && adj.nodeType === Node.ELEMENT_NODE && !adj.textContent)
        sel.collapse(fn, fwd ? oldOff + 1 : oldOff - 1);
    }
  }

  /** Get a rect with real dimensions at a range's start position. */
  _charRect(range) {
    const rect = this._rangeRectAt(range.startContainer, range.startOffset);
    if (rect) return rect;
    const r = range.getBoundingClientRect();
    return r?.height ? r : null;
  }

  /** Resolve a range from caretRangeFromPoint to a visible text node, or null. */
  _resolveToText(range) {
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
    const targetIdx = curLineIdx >= 0 ? (fwd ? curLineIdx + 1 : curLineIdx - 1) : -1;
    if (targetIdx >= 0 && targetIdx < lines.length)
      return this._pickPositionOnLine(lines[targetIdx], goalX);

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
    const caretX = sel.getRangeAt(0).getBoundingClientRect().left;
    const vp = this._viewportRect();
    const delta = this._viewportHeight - PAGE_OVERLAP;
    this._scrollBy(direction === "down" ? delta : -delta);
    const targetY = direction === "down"
      ? vp.top + PAGE_OVERLAP / 2 : vp.bottom - PAGE_OVERLAP / 2;
    const range = this._probeTextAt(caretX, targetY);
    if (!range) return;
    this._applyRange(sel, range);
    this._updateCursor();
  }

  /* ── boundary detection ──────────────────────────────────────── */

  _isAtVisibleBoundary(direction) {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return false;
    const node = sel.focusNode, off = sel.focusOffset;
    const fwd = direction === "forward";
    if (node.nodeType !== Node.TEXT_NODE) return false;
    if (!node.textContent.trim()) return !this._walkToVisible(node, fwd);
    if (fwd ? off < node.length - 1 : off > 0) return false;
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
      const t = tw.currentNode;
      if (!t.textContent.trim()) continue;
      const r = document.createRange();
      r.selectNodeContents(t);
      const rects = r.getClientRects();
      const clientRect = rects[0] || r.getBoundingClientRect();
      if (!clientRect || !clientRect.height || !clientRect.width) {
        continue;
      }
      const rect = {
        top: clientRect.top,
        left: clientRect.left,
        width: clientRect.width,
        height: clientRect.height
      };
      nodes.push({ node: t, rect });
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
    const fn = sel.focusNode;
    const el = fn.nodeType === Node.ELEMENT_NODE ? fn : fn.parentElement;
    const page = el?.closest('.page[data-page-number]');
    const scopeRoot = page || el?.closest('.textLayer') || this._root;
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    const curIdx = ordered.findIndex(e => e.node === fn);
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
      const rep = currentLine[0].rect;
      const entryMid = entry.rect.top + entry.rect.height / 2;
      const repMid = rep.top + rep.height / 2;
      const tolerance = Math.max(entry.rect.height, rep.height) / 2;
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
      const rep = lines[i][0].rect;
      const lineMid = rep.top + rep.height / 2;
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
      const resolved = this._resolveToText(probe);
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
        r.setStart(node, off); r.setEnd(node, Math.min(off + 1, node.length));
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
    const walkRoot = root || this._root;
    const tw = document.createTreeWalker(walkRoot, NodeFilter.SHOW_TEXT);
    let bestNode = null, bestTop = null, bestLeft = null;
    while (tw.nextNode()) {
      const t = tw.currentNode;
      if (!t.textContent.trim()) continue;
      const off = toStart ? 0 : Math.max(0, t.length - 1);
      const rect = this._rangeRectAt(t, off);
      if (!rect) continue;
      if (bestNode === null) {
        bestNode = t; bestTop = rect.top; bestLeft = rect.left;
        continue;
      }
      const dy = rect.top - bestTop;
      const better = toStart
        ? (dy < -2 || (Math.abs(dy) <= 2 && rect.left < bestLeft))
        : (dy > 2 || (Math.abs(dy) <= 2 && rect.left > bestLeft));
      if (better) {
        bestNode = t; bestTop = rect.top; bestLeft = rect.left;
      }
    }
    let range;
    if (bestNode) {
      range = this._collapsedRange(bestNode, toStart ? 0 : Math.max(0, bestNode.length - 1));
    } else {
      range = document.createRange();
      range.selectNodeContents(walkRoot);
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

  beginningOfPage() { const p = this._currentPage(); if (p) this._jumpToEdge(true, p); }
  endOfPage() { const p = this._currentPage(); if (p) this._jumpToEdge(false, p); }

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
