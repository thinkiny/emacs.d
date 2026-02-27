/** CaretEmacs – Emacs-like caret navigation for WebKit. */

const PAGE_OVERLAP = 40;
const CURSOR_ID = "__caret-emacs-cursor";
const STYLE_ID = "__caret-emacs-style";

const CURSOR_CSS = `
#${CURSOR_ID}{
  position:fixed;pointer-events:none;z-index:2147483647;
  background:highlight;display:none;box-sizing:border-box;
  animation:__caret-emacs-blink 1s step-end infinite
}
@keyframes __caret-emacs-blink{50%{opacity:0}}
`.trim();

class CaretEmacs {
  constructor(el = document, opts = {}) {
    this.el = el;
    this.onMark = opts.onMark || null;
    this.markActive = false;
    this._onSelectionChange = this._updateCursor.bind(this);
    this.scrollContainer = opts.scrollContainer || null;
    this._cursorEl = null;

    const init = () => {
      this._initCursor();
      this.enable();
      this._ensureSelection();
      this._updateCursor();
    };
    document.body ? init() : document.addEventListener("DOMContentLoaded", init, { once: true });
  }

  get _root() { return this.el === document ? document.body : this.el; }

  _isContained(node) {
    return node && this._root.contains(node);
  }

  /* ── scroll / viewport abstraction ───────────────────────── */

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

  /* ── lifecycle ────────────────────────────────────────────── */

  enable() {
    document.addEventListener("selectionchange", this._onSelectionChange);
  }

  disable() {
    document.removeEventListener("selectionchange", this._onSelectionChange);
    if (this.markActive) this._setMark(false);
  }

  destroy() {
    this.disable();
    this._removeCursor();
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

  _ensureSelection() {
    const sel = window.getSelection();
    if (sel?.rangeCount > 0) {
      this._relocateIfOffscreen(sel);
      return sel;
    }
    const range = document.createRange();
    range.selectNodeContents(this._root);
    range.collapse(true);
    sel.removeAllRanges();
    sel.addRange(range);
    return sel;
  }

  /** Re-place the caret at visible content when it is outside the viewport. */
  _relocateIfOffscreen(sel) {
    const r = sel.getRangeAt(0).cloneRange();
    r.collapse(true);
    const rect = r.getBoundingClientRect();
    const vp = this._viewportRect();
    if (rect.height && rect.bottom >= vp.top && rect.top <= vp.bottom) return;
    const cx = (vp.left + vp.right) / 2;
    const cy = (vp.top + vp.bottom) / 2;
    const range = document.caretRangeFromPoint(cx, cy);
    if (!range || !this._isContained(range.startContainer)) return;
    const resolved = this._resolveToText(range);
    if (resolved) {
      sel.removeAllRanges();
      sel.addRange(resolved);
    }
  }

  _setMark(active) {
    this.markActive = active;
    if (!active) {
      const sel = window.getSelection();
      if (sel?.rangeCount) sel.collapseToEnd();
    }
    this.onMark?.(active);
  }

  /** Scroll the viewport to keep the selection focus visible. */
  _scrollToSelection() {
    const sel = window.getSelection();
    if (!sel?.rangeCount) return;
    const { top, bottom } = this._collapsedRange(sel.focusNode, sel.focusOffset).getBoundingClientRect();
    const vp = this._viewportRect();
    if (bottom > vp.bottom - PAGE_OVERLAP) {
      if (Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight) return;
      this._scrollBy(bottom - vp.bottom + PAGE_OVERLAP);
    } else if (top < vp.top + PAGE_OVERLAP) {
      if (Math.floor(this._scrollTop) <= 0) return;
      this._scrollBy(top - vp.top - PAGE_OVERLAP);
    }
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
    if (!sel?.rangeCount) return void (el.style.display = "none");
    if (!this._isContained(sel.focusNode)) return void (el.style.display = "none");
    const { node, offset } = this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    const rect = this._cursorRectAt(node, offset);
    if (!rect?.height) return void (el.style.display = "none");
    const cw = this._cursorWidth(rect);
    Object.assign(el.style, {
      display: "block",
      left: `${rect.left}px`,
      top: `${rect.top}px`,
      width: `${cw}px`,
      height: `${rect.height}px`,
    });
  }

  _removeCursor() {
    document.getElementById(CURSOR_ID)?.remove();
    document.getElementById(STYLE_ID)?.remove();
    this._cursorEl = null;
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

  /** After forward-word, adjust caret to land on word start (Emacs behavior). */
  _adjustForwardWord(sel, startedOnWs) {
    const node = sel.focusNode;
    if (!node || node.nodeType !== Node.TEXT_NODE) return;
    const off = sel.focusOffset;
    const text = node.textContent;

    if (startedOnWs) {
      // WebKit crossed ws + word in one step; back up to word start.
      const before = text.slice(0, off);
      const m = before.match(/\s\S*$/);
      sel.collapse(node, m ? m.index + 1 : 0);
      return;
    }
    // At end of text node: jump to next visible text node.
    if (off >= text.length) {
      const t = this._walkToVisible(node, true);
      if (!t) return;
      const tIdx = t.textContent.search(/\w/);
      sel.collapse(t, tIdx >= 0 ? tIdx : 0);
      return;
    }
    // Hyphen inside compound word: jump to end of full compound (e.g. "well-known").
    // WebKit stops at the hyphen, so off typically points at '-'.
    const compound = text.slice(off).match(/^(-\w+)+/);
    const endOff = compound ? off + compound[0].length : off;
    // Skip non-word characters (punctuation + whitespace) to next word start.
    if (/\w/.test(text[endOff])) { if (compound) sel.collapse(node, endOff); return; }
    const idx = text.slice(endOff).search(/\w/);
    if (idx >= 0) { sel.collapse(node, endOff + idx); return; }
    const t = this._walkToVisible(node, true);
    if (!t) return;
    const tIdx = t.textContent.search(/\w/);
    sel.collapse(t, tIdx >= 0 ? tIdx : 0);
  }

  /** After backward-word, ensure caret lands on word start (Emacs behavior). */
  _skipToWordStartBackward(sel) {
    const node = sel.focusNode;
    if (!node || node.nodeType !== Node.TEXT_NODE) return;
    const off = sel.focusOffset, text = node.textContent;
    if (off < text.length && !/\s/.test(text[off]) &&
      (off === 0 || /\s/.test(text[off - 1]))) return;

    const wordStart = (txt) => {
      const m = txt.match(/.*\s(\S)/s);
      return m ? m.index + m[0].length - 1 : (/\S/.test(txt[0] || '') ? 0 : -1);
    };
    const pos = wordStart(text.slice(0, Math.min(off, text.length)));
    if (pos >= 0) { sel.collapse(node, pos); return; }

    const prev = this._walkToVisible(node, false);
    if (!prev) return;
    const pp = wordStart(prev.textContent);
    sel.collapse(prev, Math.max(0, pp));
  }

  _move(direction, granularity) {
    const sel = this._ensureSelection();
    if (!sel) return false;
    this._hitBoundary = false;
    if (this._isAtVisibleBoundary(direction)) {
      this._hitBoundary = true;
      return false;
    }

    // Pre-snap: if on whitespace node, snap to visible text first
    const { node: snapN, offset: snapO } =
      this._resolveCursorPosition(sel.focusNode, sel.focusOffset);
    if (snapN !== sel.focusNode || snapO !== sel.focusOffset)
      sel.collapse(snapN, snapO);

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
      this._unstick(sel, direction, fn0, fo0);
      return sel.focusNode !== fn0 || sel.focusOffset !== fo0;
    };

    const initNode = sel.focusNode;
    const initOff = sel.focusOffset;

    // Save anchor for mark-active; collapse focus for stepping
    const an = this.markActive ? sel.anchorNode : null;
    const ao = this.markActive ? sel.anchorOffset : null;
    if (this.markActive) sel.collapse(sel.focusNode, sel.focusOffset);

    // WebKit's forward-word from ws crosses gap + next word; track for adjustment.
    const startedOnWs = granularity === "word" && fwd &&
      sel.focusNode.nodeType === Node.TEXT_NODE &&
      sel.focusOffset < sel.focusNode.textContent.length &&
      /\s/.test(sel.focusNode.textContent[sel.focusOffset]);

    if (granularity === "line" && this.scrollContainer) {
      const lineRange = this._moveLine(fwd);
      if (lineRange) {
        if (this.markActive) {
          sel.setBaseAndExtent(an, ao, lineRange.startContainer, lineRange.startOffset);
        } else {
          sel.removeAllRanges();
          sel.addRange(lineRange);
        }
        this._scrollToSelection();
        this._updateCursor();
        return true;
      }
      this._hitBoundary = true;
      return false;
    }

    let moved = step();
    while (moved && sel.focusNode.nodeType === Node.ELEMENT_NODE) {
      if (!step()) break;
    }
    this._snapToText(sel, fwd);
    if (moved && granularity === "word") {
      if (fwd) this._adjustForwardWord(sel, startedOnWs);
      else this._skipToWordStartBackward(sel);
    }
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
    if (!moved && this._fallbackToAdjacentText(sel, fwd)) return true;

    this._scrollToSelection();
    this._updateCursor();
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
    this._scrollToSelection();
    this._updateCursor();
    return true;
  }

  /** Nudge the caret past elements that sel.modify cannot cross. */
  _unstick(sel, direction, oldNode, oldOff) {
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
    const r0 = sel.getRangeAt(0).cloneRange();
    r0.collapse(true);
    const rect = this._charRect(r0);
    if (!rect?.height) return null;
    const lh = rect.height;
    const goalX = rect.left;

    // Phase 1: move within current page
    const page = this._currentPage();
    const scopeRoot = page || this._root;
    const ordered = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    const lines = this._groupIntoLines(ordered, lh);
    if (!lines.length) return null;
    const curLineIdx = this._findCaretLine(lines, rect, lh);
    if (curLineIdx >= 0) {
      const targetIdx = fwd ? curLineIdx + 1 : curLineIdx - 1;
      if (targetIdx >= 0 && targetIdx < lines.length)
        return this._pickPositionOnLine(lines[targetIdx], goalX, lh);
    }

    // Phase 2: cross page boundary
    if (!page) return null;
    const adj = this._visuallyAdjacentPage(page, fwd);
    if (!adj) return null;
    const adjRect = adj.getBoundingClientRect();
    const vp = this._viewportRect();
    if (adjRect.bottom < vp.top || adjRect.top > vp.bottom)
      adj.scrollIntoView({ block: fwd ? 'start' : 'end' });
    const adjOrdered = this._visuallyOrderedTextNodes(adj);
    if (!adjOrdered.length) return null;
    const adjLines = this._groupIntoLines(adjOrdered, lh);
    if (!adjLines.length) return null;
    const targetLine = fwd ? adjLines[0] : adjLines[adjLines.length - 1];
    return this._pickPositionOnLine(targetLine, goalX, lh);
  }

  /** Page down/up: scroll viewport, then re-place caret using caretRangeFromPoint. */
  _page(direction) {
    const sel = this._ensureSelection();
    if (!sel?.rangeCount) return;
    const caretX = sel.getRangeAt(0).getBoundingClientRect().left;
    const vp = this._viewportRect();
    const delta = this._viewportHeight - PAGE_OVERLAP;
    this._scrollBy(direction === "down" ? delta : -delta);
    const targetY = direction === "down"
      ? vp.top + PAGE_OVERLAP / 2 : vp.bottom - PAGE_OVERLAP / 2;
    const range = document.caretRangeFromPoint(caretX, targetY);
    if (!range || !this._isContained(range.startContainer)) return;
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
    return !this._walkToVisible(node, fwd);
  }

  isAtBottom() {
    return Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight;
  }

  isAtTop() {
    return Math.floor(this._scrollTop) <= 0;
  }

  /* ── public API (for programmatic / Emacs xwidget dispatch) ── */

  forward(granularity) {
    this._lastMoved = this._move("forward", granularity);
    this._lastDir = "forward";
  }
  backward(granularity) {
    this._lastMoved = this._move("backward", granularity);
    this._lastDir = "backward";
  }
  pageDown() { this._page("down"); }
  pageUp() { this._page("up"); }
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
      const rect = r.getBoundingClientRect();
      if (!rect.height || !rect.width) continue;
      nodes.push({ node: t, rect });
    }
    nodes.sort((a, b) => {
      const dy = a.rect.top - b.rect.top;
      if (Math.abs(dy) > 2) return dy;
      return a.rect.left - b.rect.left;
    });
    return nodes;
  }

  _groupIntoLines(orderedNodes, lh) {
    if (!orderedNodes.length) return [];
    const lines = [];
    let currentLine = [orderedNodes[0]];
    let lineTop = orderedNodes[0].rect.top;
    for (let i = 1; i < orderedNodes.length; i++) {
      const entry = orderedNodes[i];
      if (Math.abs(entry.rect.top - lineTop) <= lh / 3) {
        currentLine.push(entry);
      } else {
        lines.push(currentLine);
        currentLine = [entry];
        lineTop = entry.rect.top;
      }
    }
    lines.push(currentLine);
    return lines;
  }

  _findCaretLine(lines, caretRect, lh) {
    const caretMid = caretRect.top + caretRect.height / 2;
    let bestIdx = -1, bestDist = Infinity;
    for (let i = 0; i < lines.length; i++) {
      const rep = lines[i][0].rect;
      const lineMid = rep.top + rep.height / 2;
      const dist = Math.abs(caretMid - lineMid);
      if (dist < bestDist) { bestDist = dist; bestIdx = i; }
    }
    return bestDist > lh * 3 / 2 ? -1 : bestIdx;
  }

  _pickPositionOnLine(line, goalX, lh) {
    const repRect = line[0].rect;
    const lineMidY = repRect.top + repRect.height / 2;
    // Try caretRangeFromPoint for precision
    const probe = document.caretRangeFromPoint(goalX, lineMidY);
    if (probe) {
      const resolved = this._resolveToText(probe);
      if (resolved && this._isContained(resolved.startContainer)) {
        const cr = this._charRect(resolved);
        if (cr && Math.abs(cr.top - repRect.top) < lh / 2) return resolved;
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
    const ordered = this._visuallyOrderedTextNodes(walkRoot);
    const tn = ordered.length > 0
      ? (toStart ? ordered[0].node : ordered[ordered.length - 1].node)
      : null;
    const range = tn
      ? this._collapsedRange(tn, toStart ? 0 : Math.max(0, tn.length - 1))
      : (() => { const r = document.createRange(); r.selectNodeContents(walkRoot); r.collapse(toStart); return r; })();
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

if (typeof exports !== "undefined") {
  exports.CaretEmacs = CaretEmacs;
} else if (typeof window !== "undefined") {
  window.CaretEmacs = CaretEmacs;
  const vc = document.getElementById('viewerContainer');
  const v = document.getElementById('viewer');
  if (vc && v) {
    window.__caretEmacs = new CaretEmacs(v, { scrollContainer: vc });
  } else {
    window.__caretEmacs = new CaretEmacs(document);
  }
}
