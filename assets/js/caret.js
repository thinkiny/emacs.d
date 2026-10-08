/** CaretEmacs – Emacs-like caret navigation for WebKit. */

/* ── Constants ────────────────────────────────────────────── */

const STYLE_ID = "__caret-emacs-style";
const CURSOR_TAG = "caret-cursor";
const WORD_CHAR_RE = /[\p{L}\p{N}\p{M}\p{Pc}'-]/u;
const RESTORE_TIMEOUT_MS = 10_000;
const RETRY_DELAY_MS = 100;
const PDF_INITIALIZE_DELAY_MS = 300;
const PDF_BUS_TIMEOUT_MS = 10_000;
const DEFAULT_SCROLL_PX = 200;

const cloneRect = (r) => ({
  top: r.top, bottom: r.bottom, left: r.left, right: r.right,
  width: r.width, height: r.height
});

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
    this._savedFocus = null;
    this._lastRenderedPos = null;
    this._lastMoved = false;
    this._lastDir = null;
    this._hitBoundary = false;
    this._debug = false;
    this._debugLog = [];
    this.scrollContainer = opts.scrollContainer || null;
    this._scrollPx = opts.scrollPx ?? DEFAULT_SCROLL_PX;
    this._scrollDownFraction = opts.scrollDownFraction ?? 1 / 3;
    this._scrollUpFraction = opts.scrollUpFraction ?? 2 / 3;
    this._viewportEdgeOffset = opts.viewportEdgeOffset ?? 20;
    this._cursorEl = null;
    this._scrollRaf = null;
    this._caretTask = null;
    this._pendingTextRetry = null;
    this._caretViewportTop = null;
    this._lastScrollTop = 0;
    this._suppressScrollRelocate = false;
    this._historyPositions = new Map();
    this._historyLocation = location.href;

    // Performance caches
    this._fontSizeCache = new WeakMap();
    this._textBoundsCache = new WeakMap();
    this._visualOrderCache = { root: null, layoutGeneration: -1, ordered: null, lines: null };
    this._layoutGeneration = 0;

    this._onSelectionChange = this._updateCursor.bind(this);
    this._onKeyDown = (e) => {
      if (e.ctrlKey && e.key === 'g' && !e.altKey && !e.metaKey)
        this.deactivateMark();
    };
    // A mouse click quits the mark and becomes point after the default action.
    this._onMouseDown = () => {
      if (this.markActive) this.deactivateMark();
      const task = this._startCaretTask();
      this._scheduleCaretTask(task, () => {
        const selection = window.getSelection();
        if (selection?.focusNode && this._isContained(selection.focusNode))
          this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
        this._finishCaretTask();
      }, 0);
    };
    // Direct scrolling abandons reveal work so the resulting scroll can become point.
    this._onWheel = () => {
      this._cancelCaretTask();
      this._suppressScrollRelocate = false;
    };
    this._onScroll = () => this._onUserScroll();
    // bfcache resume never re-runs restore() and restores no scroll — re-anchor.
    this._onPageShow = (e) => {
      if (!e.persisted) return;
      const savedFocus = this._savedFocus;
      const retryPosition = this._caretTask?.target != null
        || this._caretTask?.hash != null
        || !savedFocus?.node || !this._isContained(savedFocus.node)
        || !this._hasRenderedBox(savedFocus.node);
      this._cancelCaretTask();
      if (location.hash && retryPosition && this._restoreHashTarget()) return;
      if (!location.hash && retryPosition && this._restoreCaret()) return;
      if (savedFocus?.node && this._isContained(savedFocus.node)) {
        const selection = window.getSelection();
        if (!selection.rangeCount || selection.focusNode !== savedFocus.node
            || selection.focusOffset !== savedFocus.offset)
          this._setSelectionRange(selection,
            this._collapsedRange(savedFocus.node, savedFocus.offset));
      }
      this._revealCaret();
    };
    this._onHashChange = () => {
      this._cancelCaretTask();
      this._historyLocation = location.href;
      this._invalidateLayoutCaches();
      const position = this._historyPositions.get(location.href);
      if (!position || !this._restoreHistoryPosition(position)) {
        if (!this._restoreHashTarget()) {
          this._ensureSelection();
          this._revealCaret();
        }
      }
    };
    this._onResize = () => {
      if (this.scrollContainer) this._scheduleViewportResize();
      else this._scheduleResizeRestore();
    };

    const init = () => {
      this._initCursor();
      document.addEventListener("selectionchange", this._onSelectionChange);
      document.addEventListener("keydown", this._onKeyDown);
      document.addEventListener("mousedown", this._onMouseDown);
      window.addEventListener('wheel', this._onWheel, { passive: true, capture: true });
      if (this.scrollContainer) {
        this._initPdfScroll();
      } else {
        if ('scrollRestoration' in history) history.scrollRestoration = 'manual';
        this.restore();
        window.addEventListener('scroll', this._onScroll, { passive: true });
        window.addEventListener('pageshow', this._onPageShow);
        window.addEventListener('hashchange', this._onHashChange);
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
    for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
      const line = lines[lineIndex];
      const bounds = this._lineBounds(line);
      const segments = line.map(entry => {
        const left = Math.round(entry.rect.left);
        const width = Math.round(entry.rect.width);
        const height = Math.round(entry.rect.height);
        const text = (entry.node.textContent || "").slice(0, 25).replace(/\n/g, "\\n");
        const group = entry.groupRoot ? `g` : `-`;
        const segmentIndex = ordered.indexOf(entry);
        return `[${left}+${width}x${height} ${group} #${segmentIndex} "${text}"]`;
      }).join(" ");
      report.push(`L${lineIndex}: y=${Math.round(bounds.top)} x=${Math.round(bounds.left)}-${Math.round(bounds.right)} h=${Math.round(bounds.height)}  ${segments}`);
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
    else window.scrollBy({ top: dy, left: 0, behavior: 'instant' });
    const distance = (container ? container.scrollTop : window.scrollY) - before;
    if (distance) this._invalidateLayoutCaches();
    return distance;
  }

  _scrollTo(y) {
    const container = this.scrollContainer;
    const before = container ? container.scrollTop : window.scrollY;
    if (container) container.scrollTop = y;
    else window.scrollTo({ top: y, left: window.scrollX, behavior: 'instant' });
    if ((container ? container.scrollTop : window.scrollY) !== before) this._invalidateLayoutCaches();
  }

  _scrollToSelectionLineBounded() {
    const selection = window.getSelection();
    if (!selection?.rangeCount) {
      return false;
    }

    const focusRect = this._selectionFocusRect(selection);
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
    const selection = window.getSelection();
    if (!selection?.rangeCount) {
      return false;
    }

    const focusRect = this._selectionFocusRect(selection);
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

  /* ── Caret restore ────────────────────────────────────────────
   * One task owns delayed caret work. Replacing it makes stale callbacks
   * harmless and keeps restore, resize, hash, and input mutually exclusive. */

  _startCaretTask(state = {}) {
    this._cancelCaretTask();
    const task = { timer: null, ...state };
    this._caretTask = task;
    return task;
  }

  _scheduleCaretTask(task, callback, delay) {
    if (this._caretTask !== task) return;
    if (task.timer != null) clearTimeout(task.timer);
    task.timer = setTimeout(() => {
      if (this._caretTask !== task) return;
      task.timer = null;
      callback();
    }, delay);
  }

  _releaseCaretTask(task = this._caretTask) {
    if (!task) return;
    if (task.timer != null) clearTimeout(task.timer);
    task?.unsubscribe?.();
    if (this._caretTask === task) this._caretTask = null;
    this._suppressScrollRelocate = false;
  }

  _cancelCaretTask() {
    this._releaseCaretTask();
  }

  _pauseCaretTask(task) {
    if (task.timer != null) clearTimeout(task.timer);
    task.timer = null;
    task.blockRelocation = false;
    this._suppressScrollRelocate = false;
    this._lastScrollTop = this._scrollTop;
  }

  _finishCaretTask({ redraw = true, suppressNextScroll = false } = {}) {
    this._cancelCaretTask();
    this._lastRenderedPos = null;
    this._lastScrollTop = this._scrollTop;
    if (redraw) this._updateCursor();
    this._suppressScrollRelocate = suppressNextScroll;
  }

  _storedCaretPosition() {
    let stored;
    try { stored = localStorage.getItem(this._caretStorageKey()); } catch (e) { return null; }
    if (stored == null) return null;
    const raw = stored.trim();
    if (/^\d+$/.test(raw)) {
      const offset = Number(raw);
      return Number.isSafeInteger(offset) ? { offset, nextBoundary: false } : null;
    }
    try {
      const value = JSON.parse(raw);
      return Number.isSafeInteger(value.offset) && value.offset >= 0
        ? { offset: value.offset, nextBoundary: value.nextBoundary === true } : null;
    } catch (e) {
      return null;
    }
  }

  _beginStoredRestore(target, deadline = performance.now() + RESTORE_TIMEOUT_MS,
    bookmark = { globalOffset: target }) {
    const task = this._startCaretTask({ target, deadline, bookmark });
    this._suppressScrollRelocate = true;
    this._scheduleCaretTask(task, () => this._pollStoredRestore(task), 0);
  }

  /** Start restoring a valid stored offset, even before hydration resolves it. */
  _restoreCaret() {
    const position = this._storedCaretPosition();
    if (!position) return false;
    this._beginStoredRestore(position.offset, performance.now() + RESTORE_TIMEOUT_MS, {
      globalOffset: position.offset, nextBoundary: position.nextBoundary
    });
    return true;
  }

  _pollStoredRestore(task) {
    const position = this._resolveTextBookmark(task.bookmark);
    let caretRect = null;
    if (position) {
      const selection = window.getSelection();
      if (!selection.rangeCount || selection.focusNode !== position.node
          || selection.focusOffset !== position.offset)
        this._setSelectionRange(selection, this._collapsedRange(position.node, position.offset));
      this._savedFocus = { node: position.node, offset: position.offset };
      caretRect = this._selectionFocusRect(selection);
    }

    const now = performance.now();
    const visible = caretRect && this._isRectInViewport(caretRect);
    if (now >= task.deadline) {
      if (visible) {
        this._finishCaretTask();
      } else {
        this._ensureSelection();
        this._updateCursor();
        this._pauseCaretTask(task);
      }
      return;
    }
    if (caretRect) this._scrollToSelection();
    this._scheduleCaretTask(task, () => this._pollStoredRestore(task), RETRY_DELAY_MS);
  }

  /** Scroll the saved point into view, retrying until it shows or 10s pass. */
  _revealCaret() {
    const task = this._startCaretTask({
      deadline: performance.now() + RESTORE_TIMEOUT_MS
    });
    this._suppressScrollRelocate = true;
    this._scheduleCaretTask(task, () => this._pollReveal(task), 0);
  }

  _pollReveal(task) {
    const selection = window.getSelection();
    let savedFocusPoint = this._savedFocusPoint();
    if (savedFocusPoint && (savedFocusPoint.node.nodeType !== Node.TEXT_NODE
        || !this._hasRenderedBox(savedFocusPoint.node))) savedFocusPoint = null;
    if (!savedFocusPoint && selection?.focusNode && this._isContained(selection.focusNode)) {
      savedFocusPoint = this._resolveCursorPosition(selection.focusNode, selection.focusOffset);
      if (savedFocusPoint.node?.nodeType === Node.TEXT_NODE
          && this._hasRenderedBox(savedFocusPoint.node))
        this._savedFocus = { node: savedFocusPoint.node, offset: savedFocusPoint.offset };
      else
        savedFocusPoint = null;
    }
    const ready = !!savedFocusPoint;
    if (ready && (!selection.rangeCount || selection.focusNode !== savedFocusPoint.node
        || selection.focusOffset !== savedFocusPoint.offset))
      this._setSelectionRange(selection,
        this._collapsedRange(savedFocusPoint.node, savedFocusPoint.offset));
    const caretRect = ready ? this._selectionFocusRect(selection) : null;

    if (caretRect && this._isRectInViewport(caretRect)) {
      this._finishCaretTask();
      return;
    }
    if (performance.now() >= task.deadline) {
      this._ensureSelection();
      this._updateCursor();
      this._finishCaretTask({ redraw: false });
      return;
    }
    if (caretRect) this._scrollToSelection();
    this._scheduleCaretTask(task, () => this._pollReveal(task), RETRY_DELAY_MS);
  }

  _textNodePath(node) {
    if (node?.nodeType !== Node.TEXT_NODE || !this._isContained(node)) return null;
    let anchor = node.parentElement;
    while (anchor && anchor !== this._root && !anchor.id) anchor = anchor.parentElement;
    if (!anchor) return null;
    const steps = [];
    for (let current = node; current !== anchor; current = current.parentNode) {
      const parent = current.parentNode;
      const index = parent ? Array.prototype.indexOf.call(parent.childNodes, current) : -1;
      if (index < 0) return null;
      steps.unshift({
        index,
        parentCount: parent.childNodes.length,
        type: current.nodeType,
        tag: current.nodeType === Node.ELEMENT_NODE ? current.tagName : null,
        id: current.nodeType === Node.ELEMENT_NODE ? current.id || null : null      });
    }
    return { anchorId: anchor === this._root ? null : anchor.id, steps };
  }

  _resolveTextPath(path, offset) {
    if (!path) return null;
    let node = path.anchorId ? document.getElementById(path.anchorId) : this._root;
    if (!node || !this._isContained(node)) return null;
    for (const step of path.steps) {
      if (node.childNodes.length !== step.parentCount) return null;
      node = node.childNodes[step.index];
      if (!node || node.nodeType !== step.type) return null;
      if (step.tag && (node.tagName !== step.tag || (node.id || null) !== step.id))
        return null;
    }
    if (node.nodeType !== Node.TEXT_NODE || !this._hasRenderedBox(node)) return null;
    return { node, offset: Math.min(offset, node.length) };
  }

  _captureTextBookmark(point) {
    if (point?.node?.nodeType !== Node.TEXT_NODE || !this._isContained(point.node))
      return null;
    const globalOffset = this._caretGlobalOffset(point.node, point.offset);
    if (globalOffset == null) return null;
    return {
      node: point.node,
      offset: point.offset,
      path: this._textNodePath(point.node),
      globalOffset,
      nextBoundary: point.offset === 0
    };
  }

  _resolveTextBookmark(saved) {
    if (!saved) return null;
    if (saved.node?.nodeType === Node.TEXT_NODE && this._isContained(saved.node)
        && this._hasRenderedBox(saved.node)) {
      return { node: saved.node, offset: Math.min(saved.offset, saved.node.length), viaOffset: false };
    }
    const pathPoint = this._resolveTextPath(saved.path, saved.offset);
    if (pathPoint) return { ...pathPoint, viaOffset: false };
    const point = this._resolveGlobalOffset(saved.globalOffset);
    if (!point) return null;
    if (!saved.nextBoundary || point.offset !== point.node.length)
      return { ...point, viaOffset: true };
    const next = this._walkToVisible(point.node, true);
    return next
      ? { node: next, offset: 0, viaOffset: true }
      : { ...point, viaOffset: true };
  }

  _captureResizeRestore(restoreTarget, restoreNextBoundary = false) {
    const selection = window.getSelection();
    const currentPoint = this._savedFocusPoint();
    const savedPoint = this._captureTextBookmark(currentPoint);
    const livePoint = selection?.focusNode && this._isContained(selection.focusNode)
      ? { node: selection.focusNode, offset: selection.focusOffset } : null;
    const preferLive = !this.markActive && selection?.rangeCount && !selection.isCollapsed;
    let point = preferLive
      ? this._captureTextBookmark(livePoint) || savedPoint
      : savedPoint || this._captureTextBookmark(livePoint);
    if (restoreTarget != null && point?.globalOffset !== restoreTarget) {
      point = { offset: 0, globalOffset: restoreTarget, nextBoundary: restoreNextBoundary };
    }
    if (!point) {
      const stored = this._storedCaretPosition();
      if (stored) point = { offset: 0, globalOffset: stored.offset, nextBoundary: stored.nextBoundary };
    }
    if (!point) return null;
    const mark = this.markActive ? this._captureTextBookmark(this._markAnchorPoint()) : null;
    const savedCaret = this.markActive ? this._captureTextBookmark(this._savedCaret) : null;
    return { point, mark, savedCaret, markActive: this.markActive,
      viewportTop: this._caretViewportTop };
  }

  _scheduleResizeRestore() {
    let task = this._caretTask;
    if (task?.snapshot) {
      task.deadline = performance.now() + RESTORE_TIMEOUT_MS;
    } else {
      const restoreTarget = task?.target ?? null;
      const hash = task?.hash ?? null;
      const snapshot = this._captureResizeRestore(
        restoreTarget, task?.bookmark?.nextBoundary === true);
      if (!snapshot) {
        if (hash && task) {
          task.deadline = performance.now() + RESTORE_TIMEOUT_MS;
          this._invalidateLayoutCaches();
          this._suppressScrollRelocate = true;
          this._scheduleCaretTask(task, () => this._pollHashTarget(task), RETRY_DELAY_MS);
        } else {
          this._scheduleViewportResize();
        }
        return;
      }
      task = this._startCaretTask({
        snapshot, target: restoreTarget, hash,
        deadline: performance.now() + RESTORE_TIMEOUT_MS
      });
    }
    this._invalidateLayoutCaches();
    this._suppressScrollRelocate = true;
    this._lastScrollTop = this._scrollTop;
    this._scheduleCaretTask(task, () => this._restoreResizedSelection(task), RETRY_DELAY_MS);
  }

  _restoreResizedSelection(task) {
    const saved = task.snapshot;
    const point = this._resolveTextBookmark(saved.point);
    const mark = saved.markActive ? this._resolveTextBookmark(saved.mark) : null;
    const savedCaret = saved.markActive ? this._resolveTextBookmark(saved.savedCaret) : null;
    const waitingForMark = saved.markActive && saved.mark && !mark;
    const waitingForSavedCaret = saved.markActive && saved.savedCaret && !savedCaret;
    if ((!point || waitingForMark || waitingForSavedCaret)
        && performance.now() < task.deadline) {
      this._scheduleCaretTask(task, () => this._restoreResizedSelection(task), RETRY_DELAY_MS);
      return;
    }
    if (!point) {
      this._clearMark();
      this._ensureSelection();
      this._updateCursor();
      if (task.hash) {
        this._restoreHashTarget(task.hash);
      } else if (task.target != null) {
        this._pauseCaretTask(task);
      } else {
        this._cancelCaretTask();
      }
      return;
    }
    this._applyResizedSelection(saved, point, mark, savedCaret);
    if (task.hash)
      this._restoreHashTarget(task.hash);
    else if (task.target != null)
      this._beginStoredRestore(task.target, task.deadline, {
        globalOffset: task.target,
        nextBoundary: saved.point.nextBoundary === true
      });
    else
      this._revealCaret();
  }

  _applyResizedSelection(saved, point, mark, savedCaret) {
    const selection = window.getSelection();
    this._savedFocus = { node: point.node, offset: point.offset };
    if (mark) {
      this._markAnchor = { node: mark.node, offset: mark.offset };
      this._savedCaret = savedCaret
        ? { node: savedCaret.node, offset: savedCaret.offset } : null;
      this.markActive = true;
      selection.setBaseAndExtent(mark.node, mark.offset, point.node, point.offset);
    } else {
      this._restorePoint(point);
    }
    this._restoreViewportTop(saved.viewportTop);
  }

  _clearMark() {
    this.markActive = false;
    this._markAnchor = null;
    this._savedCaret = null;
  }

  _restorePoint(point) {
    this._clearMark();
    this._applyProbeResult(this._collapsedRange(point.node, point.offset));
  }

  _restoreViewportTop(viewportTop, boundedFallback = false) {
    const selection = window.getSelection();
    const rect = selection?.rangeCount ? this._selectionFocusRect(selection) : null;
    if (rect && viewportTop != null) {
      const delta = (rect.top - this._viewportRect().top) - viewportTop;
      return Math.abs(delta) >= 1 && !!this._scrollBy(delta);
    }
    return boundedFallback ? this._scrollToSelectionLineBounded() : false;
  }

  _scheduleViewportResize() {
    let task = this._caretTask;
    if (task?.unsubscribe) return;
    if (!task?.viewportOnly) {
      task = this._startCaretTask({
        viewportOnly: true, viewportTop: this._caretViewportTop
      });
    }
    this._suppressScrollRelocate = true;
    this._scheduleCaretTask(task, () => {
      this._invalidateLayoutCaches();
      const scrolled = this._restoreViewportTop(task.viewportTop, true);
      this._finishCaretTask({ suppressNextScroll: scrolled });
    }, RETRY_DELAY_MS);
  }

  _restoreDefaultCaret() {
    if (this._restoreCaret()) return;
    this._ensureSelection();
    this._revealCaret();
  }

  /** Restore the caret, preferring native fragment navigation. */
  restore() {
    if (location.hash && this._restoreHashTarget()) return;
    this._restoreDefaultCaret();
  }

  /** Restore point and scroll for a previously visited full URL. */
  _restoreHistoryPosition(position) {
    const point = this._resolveTextBookmark(position);
    if (!point) return false;
    this._restorePoint(point);
    this._revealCaret();
    return true;
  }

  _decodedHash() {
    const hash = location.hash.slice(1);
    try { return decodeURIComponent(hash); } catch (e) { return hash; }
  }

  /** Synchronize point to a hash target while native navigation owns scrolling. */
  _restoreHashTarget(hash = this._decodedHash()) {
    if (!hash) return false;
    // SPA route fragments (#/path) never resolve to an element — don't hold
    // the ten-second poll open for them.
    if (hash.startsWith('/')) return false;
    const task = this._startCaretTask({
      hash, deadline: performance.now() + RESTORE_TIMEOUT_MS
    });
    this._suppressScrollRelocate = true;
    this._pollHashTarget(task);
    return true;
  }

  _firstRenderedTextWithin(target) {
    const walker = document.createTreeWalker(target, NodeFilter.SHOW_TEXT);
    while (walker.nextNode()) {
      const node = walker.currentNode;
      if (this._isNavigableTextNode(node) && this._hasRenderedBox(node))
        return { node, offset: this._textVisibleEdgeOffset(node, true) };
    }
    return null;
  }

  _firstRenderedTextAfter(target) {
    const node = this._walkToVisible(target, true);
    return node ? { node, offset: this._textVisibleEdgeOffset(node, true) } : null;
  }

  _pollHashTarget(task) {
    const target = document.getElementById(task.hash)
      || document.getElementsByName(task.hash)[0];
    if (!target && task.hash.toLowerCase() === 'top') {
      this._cancelCaretTask();
      this._jumpToEdge(true);
      return;
    }
    let resolved = target && this._isContained(target)
      ? this._firstRenderedTextWithin(target) || this._firstRenderedTextAfter(target)
      : null;
    if (resolved) {
      this._restorePoint(resolved);
      this._revealCaret();
      return;
    }
    if (performance.now() >= task.deadline) {
      this._cancelCaretTask();
      if (target) {
        this._ensureSelection();
        this._updateCursor();
      } else {
        this._restoreDefaultCaret();
      }
      return;
    }
    this._scheduleCaretTask(task, () => this._pollHashTarget(task), RETRY_DELAY_MS);
  }

  /** Page down/up: scroll viewport, then re-place caret using caretRangeFromPoint. */
  _scrollPage(direction) {
    this._cancelCaretTask();
    const selection = this._ensureSelection();
    if (!selection?.rangeCount) return;
    // Under mark, getRangeAt(0) is the region bbox — judge visibility by the
    // real caret (_savedFocus) so an on-screen caret isn't relocated eagerly.
    let rect;
    if (this.markActive && this._savedFocus && this._isContained(this._savedFocus.node)) {
      rect = this._rangeRectAt(this._savedFocus.node, this._savedFocus.offset)
        || this._collapsedRange(this._savedFocus.node, this._savedFocus.offset).getBoundingClientRect();
    } else {
      rect = selection.getRangeAt(0).getBoundingClientRect();
    }
    if (!rect) return;
    const isForward = direction === "down";
    const delta = this._viewportHeight / 3;
    this._suppressScrollRelocate = true;
    const requested = isForward ? delta : -delta;
    const moved = this._scrollBy(requested);
    if (!moved) {
      this._suppressScrollRelocate = false;
      this._updateCursor();
      return;
    }
    // The old rect is static, so translate it by the actual scroll distance.
    const newTop = rect.top - moved;
    const newBottom = rect.bottom - moved;
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
    const placeCaret = () => {
      if (!this._cursorEl) return;
      this._ensureSelection();
      this._updateCursor();
    };
    const pollDeadline = performance.now() + PDF_BUS_TIMEOUT_MS;
    const poll = () => {
      if (!this._cursorEl) return;
      const bus = window.PDFViewerApplication?.eventBus;
      // A container without a pdf.js event bus never produces one — stop
      // polling at the deadline instead of spinning forever.
      if (!bus) {
        if (performance.now() < pollDeadline) requestAnimationFrame(poll);
        return;
      }
      let placed = false;
      const ensureCaret = () => {
        if (placed || !this._cursorEl) return;
        placed = true;
        const pageNum = window.PDFViewerApplication?.page;
        if (document.querySelector(`.page[data-page-number="${pageNum}"] .textLayer`)?.children.length) {
          placeCaret();
          return;
        }
        this._onTextLayerReady(placeCaret, pageNum);
      };
      this._onPdfScroll = () => {
        ensureCaret();
        this._onUserScroll();
      };
      this.scrollContainer.addEventListener('scroll', this._onPdfScroll, { passive: true });
      setTimeout(ensureCaret, PDF_INITIALIZE_DELAY_MS);
    };
    poll();
  }

  /** Call callback on the next textlayerrendered event (or immediately if no event bus). */
  _onTextLayerReady(callback, pageNumber = null) {
    const bus = window.PDFViewerApplication?.eventBus;
    if (!bus) { if (this._cursorEl) callback(); return () => {}; }
    const listener = (event) => {
      if (pageNumber != null && event?.pageNumber !== pageNumber) return;
      bus.off('textlayerrendered', listener);
      if (!this._cursorEl) return;
      this._invalidateLayoutCaches();
      callback();
    };
    bus.on('textlayerrendered', listener);
    return () => bus.off('textlayerrendered', listener);
  }

  _onUserScroll() {
    if (this._scrollRaf != null) return;
    this._scrollRaf = requestAnimationFrame(() => {
      this._scrollRaf = null;
      this._invalidateLayoutCaches();
      const scrollTop = this._scrollTop;
      const isForward = scrollTop > this._lastScrollTop;
      // Scroll changed screen positions — force cursor redraw even if DOM position is same
      if (scrollTop !== this._lastScrollTop) this._lastRenderedPos = null;
      this._lastScrollTop = scrollTop;
      if (this._caretTask && this._caretTask.blockRelocation !== false) return;

      const selection = window.getSelection();
      if (!selection?.rangeCount) {
        this._ensureSelection();
        this._updateCursor();
        return;
      }

      const viewportRect = this._viewportRect();
      const caretRect = this._isContained(selection.focusNode)
        ? this._rangeRectAt(selection.focusNode, selection.focusOffset) : null;
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
      } else if (this._isPdfMode()) {
        const probePage = document.elementFromPoint(probeX, probeY)
          ?.closest('.page[data-page-number]');
        const pageNumber = probePage?.dataset?.pageNumber
          ? Number(probePage.dataset.pageNumber)
          : window.PDFViewerApplication?.page ?? null;
        if (this._pendingTextRetry?.pageNumber !== pageNumber) {
          this._pendingTextRetry?.cancel?.();
          this._pendingTextRetry = null;
        }
        if (!this._pendingTextRetry) {
          const retry = { pageNumber, cancel: null };
          this._pendingTextRetry = retry;
          retry.cancel = this._onTextLayerReady(() => {
            if (this._pendingTextRetry !== retry) return;
            this._pendingTextRetry = null;
            const range = this._probeWithFallback(probeX, probeY, isForward, viewportRect);
            if (range) this._applyProbeResult(range);
            this._updateCursor();
          }, pageNumber);
        }
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
    const selection = window.getSelection();
    if (!selection?.rangeCount) {
      // Selection lost — restore from the last rendered position.
      const saved = this._lastRenderedPos;
      if (saved?.node && this._root.contains(saved.node)) {
        selection.removeAllRanges();
        selection.addRange(this._collapsedRange(saved.node, saved.offset));
      }
      if (!selection?.rangeCount) { el.style.display = "none"; return; }
    }
    // A reveal may have drawn this position while persistence was protected.
    const lastRendered = this._lastRenderedPos;
    if (lastRendered && selection.focusNode === lastRendered.node && selection.focusOffset === lastRendered.offset
        && el.style.display === "block") {
      if (!this.scrollContainer && !this._caretTask)
        this._saveCaretPosition();
      return;
    }

    if (!this._isContained(selection.focusNode)) { el.style.display = "none"; return; }
    // Render at the actual focus when it has a usable rect — whitespace-only
    // nodes are real positions; resolving them backward lands on the prior
    // line and makes moves look stuck.
    let node = selection.focusNode, offset = selection.focusOffset;
    let rect = node.nodeType === Node.TEXT_NODE ? this._cursorRectAt(node, offset) : null;
    if (!rect?.height) {
      const resolved = this._resolveCursorPosition(selection.focusNode, selection.focusOffset);
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
    this._lastRenderedPos = { node: selection.focusNode, offset: selection.focusOffset };
    // Track viewport-relative Y for resize re-anchoring.
    this._caretViewportTop = rect.top - this._viewportRect().top;
    // Persist only when no active task protects an earlier stored target.
    if (!this.scrollContainer && !this._caretTask) {
      this._saveCaretPosition();
    }
  }

  /** Get a client rect for cursor display at the given text position. */
  _cursorRectAt(node, offset) {
    const rect = this._rangeRectAt(node, offset);
    if (rect) return rect;
    // Try previous char if it's on the same visual line
    if (offset > 0) {
      const previousRect = this._rangeRectAt(node, offset - 1);
      const characterRect = this._collapsedRange(node, offset).getBoundingClientRect();
      const lineHeight = Math.max(previousRect?.height || 0, characterRect?.height || 0);
      if (previousRect && characterRect
          && Math.abs(previousRect.top - characterRect.top) <= lineHeight) {
        return previousRect;
      }
    }
    // Final fallback: collapsed range position
    const collapsedRange = this._collapsedRange(node, offset);
    return collapsedRange.getClientRects()[0] || collapsedRange.getBoundingClientRect();
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

  /** Walk from `node` in direction `forward`, returning the first visible text node, or null. */
  _walkToVisible(node, forward) {
    if (!this._isContained(node)) return null;
    const walker = this._textWalker(node);
    let textNode = forward ? walker.nextNode() : walker.previousNode();
    while (textNode && !(this._isNavigableTextNode(textNode) && this._hasRenderedBox(textNode))) {
      textNode = forward ? walker.nextNode() : walker.previousNode();
    }
    return textNode;
  }

  /** True if moving from the first point to the second goes the wrong way. */
  _movedWrongWay(startNode, startOffset, targetNode, targetOffset, forward) {
    try {
      const startRange = document.createRange();
      startRange.setStart(startNode, startOffset);
      const targetRange = document.createRange();
      targetRange.setStart(targetNode, targetOffset);
      const comparison = startRange.compareBoundaryPoints(Range.START_TO_START, targetRange);
      return (forward && comparison > 0) || (!forward && comparison < 0);
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
      // SVG text renders, but WebKit gives its sub-ranges zero client rects,
      // so the caret can never land on it — keep it out of the model.
      if (el.namespaceURI === "http://www.w3.org/2000/svg") return false;
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

  _renderedTextDescendant(element, atStart) {
    if (!element || element.nodeType !== Node.ELEMENT_NODE) return null;
    const walker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT);
    let candidate = null;
    let textNode;
    while ((textNode = walker.nextNode())) {
      if (!this._isNavigableTextNode(textNode) || !this._hasRenderedBox(textNode)) continue;
      if (atStart) return textNode;
      candidate = textNode;
    }
    return candidate;
  }

  /** Resolve element/whitespace focus to a visible text position. */
  _resolveCursorPosition(node, offset, preferForward = false) {
    if (node.nodeType === Node.ELEMENT_NODE) {
      const atEnd = offset >= node.childNodes.length;
      const child = atEnd ? node.lastChild : node.childNodes[offset];
      const boundaryTextNode = child?.nodeType === Node.TEXT_NODE
        && this._isNavigableTextNode(child) && this._hasRenderedBox(child)
        ? child
        : this._renderedTextDescendant(child, !atEnd);
      if (boundaryTextNode)
        return { node: boundaryTextNode,
          offset: this._textVisibleEdgeOffset(boundaryTextNode, !atEnd) };

      const start = child || node;
      const textNode = this._walkToVisible(start, true) || this._walkToVisible(start, false);
      if (textNode) return { node: textNode, offset: this._textVisibleEdgeOffset(textNode, true) };
    }
    if (node.nodeType === Node.TEXT_NODE &&
      (!this._isNavigableTextNode(node) || !this._hasRenderedBox(node))) {
      // When probing (preferForward), walk forward first — backward can cross
      // page boundaries to far-away text.
      const first = this._walkToVisible(node, preferForward);
      if (first) return { node: first, offset: this._textVisibleEdgeOffset(first, preferForward) };
      const second = this._walkToVisible(node, !preferForward);
      if (second) return { node: second, offset: this._textVisibleEdgeOffset(second, !preferForward) };
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
      const length = text.length;
      const clampedOffset = Math.max(0, Math.min(offset, length));
      if (clampedOffset > 0 && clampedOffset < length && text[clampedOffset] === '\n') {
        const previousRect = this._rangeRectAt(node, clampedOffset - 1);
        if (previousRect) return previousRect;
      }
      const currentRect = this._rangeRectAt(node, clampedOffset);
      if (currentRect) return currentRect;
      if (clampedOffset > 0) {
        const previousRect = this._rangeRectAt(node, clampedOffset - 1);
        if (previousRect) return previousRect;
      }
      const fallback = this._collapsedRange(node, clampedOffset).getBoundingClientRect();
      return fallback?.height ? fallback : null;
    }
    return this._charRect(this._collapsedRange(node, offset));
  }

  /** Resolve the current selection focus to a visible rect, or null. */
  _selectionFocusRect(selection) {
    if (!selection?.focusNode) return null;
    const focusRect = this._rangeRectAt(selection.focusNode, selection.focusOffset)
      || this._collapsedRange(selection.focusNode, selection.focusOffset).getBoundingClientRect();
    return focusRect?.height ? focusRect : null;
  }

  /** True when rect overlaps the viewport vertically. */
  _isRectInViewport(rect, viewportRect = this._viewportRect()) {
    if (!rect?.height) return false;
    const rectBottom = rect.bottom ?? (rect.top + rect.height);
    return rectBottom >= viewportRect.top && rect.top <= viewportRect.bottom;
  }

  /** Replace selection with a single range. */
  _setSelectionRange(selection, range) {
    selection.removeAllRanges();
    selection.addRange(range);
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
    const seenPinned = new Set();
    while (true) {
      const pinned = this._pinnedAncestor(range.startContainer);
      if (!pinned) break;
      if (seenPinned.has(pinned)) return null;
      seenPinned.add(pinned);
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
    // Reject probes whose resolved box is well outside the hit-tested point.
    if (rangeRect && Math.abs(rangeRect.top - effectiveCy) > this._viewportHeight) return null;
    return resolved;
  }

  /** Find the first/last navigable text span in the visible viewport (PDF mode). */
  _findVisibleText(isForward, viewportRect, preferX) {
    const pages = this.el.querySelectorAll('.page');
    let best = null;
    let bestY = isForward ? Infinity : -Infinity;
    let bestXDist = Infinity;
    for (const page of pages) {
      const pageRect = page.getBoundingClientRect();
      if (pageRect.bottom < viewportRect.top || pageRect.top > viewportRect.bottom) continue;
      const textLayer = page.querySelector('.textLayer');
      if (!textLayer) continue;
      const spans = textLayer.querySelectorAll('span');
      for (const span of spans) {
        const spanRect = span.getBoundingClientRect();
        if (spanRect.bottom < viewportRect.top || spanRect.top > viewportRect.bottom) continue;
        const textNode = span.firstChild;
        if (textNode?.nodeType !== Node.TEXT_NODE || !this._isNavigableTextNode(textNode)) continue;
        const y = spanRect.top;
        const xDist = preferX != null ? Math.abs(spanRect.left - preferX) : 0;
        // Pick best Y; on Y tie, pick closest X to preferX
        const betterY = isForward ? y < bestY : y > bestY;
        const sameY = Math.abs(y - bestY) < spanRect.height;
        if (betterY || (sameY && xDist < bestXDist)) {
          bestY = y;
          bestXDist = xDist;
          best = textNode;
        }
      }
    }
    if (!best) return null;
    const offset = isForward
      ? this._textVisibleEdgeOffset(best, true) : this._textVisibleEdgeOffset(best, false);
    return this._collapsedRange(best, offset);
  }

  /** Probe at (cx,cy); if nothing found and in PDF mode, fall back to _findVisibleText. */
  _probeWithFallback(cx, cy, isForward, viewportRect) {
    let range = this._probeTextAt(cx, cy);
    if (!range && this._isPdfMode()) {
      range = this._findVisibleText(isForward, viewportRect || this._viewportRect(), cx);
    }
    if (!range) return null;
    const rect = this._rangeRectAt(range.startContainer, range.startOffset);
    return this._isRectInViewport(rect, viewportRect || this._viewportRect()) ? range : null;
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

  _savedFocusPoint() {
    const point = this._savedFocus;
    if (!point?.node || !this._isContained(point.node)) return null;
    const max = point.node.nodeType === Node.TEXT_NODE
      ? point.node.length : point.node.childNodes.length;
    return { node: point.node, offset: Math.min(point.offset, max) };
  }

  /** Apply a probed range to the selection and save focus. Under mark, the
   *  region is re-rendered from state (mark ↔ point). */
  _applyProbeResult(range) {
    const selection = window.getSelection();
    this._setSelectionRange(selection, range);
    this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
    const mark = this.markActive ? this._markAnchorPoint() : null;
    if (mark) {
      selection.setBaseAndExtent(mark.node, mark.offset, selection.focusNode, selection.focusOffset);
    }
    this._logDebug("probe-apply", {
      rangeCount: selection?.rangeCount ?? 0,
      markActive: this.markActive
    });
  }

  /** Probe one visual line below/above a caret rect (fallback for _moveLine). */
  _probeLineFromCaret(forward, caretRect) {
    if (!caretRect) return null;
    const step = (caretRect.height || 18) + 6;
    const probeX = caretRect.left + (caretRect.width || 1) / 2;
    const probeY = forward ? caretRect.bottom + step : caretRect.top - step;
    return this._probeTextAt(probeX, probeY);
  }

  /* ── Selection Management ──────────────────────────────────── */

  _ensureSelection(skipRelocate = false) {
    const selection = window.getSelection();
    if (selection?.rangeCount > 0) {
      if (!skipRelocate) this._relocateIfOffscreen(selection);
      return selection;
    }
    const saved = this._savedFocus;
    if (saved?.node && this._root.contains(saved.node)) {
      const range = this._collapsedRange(saved.node, saved.offset);
      selection.removeAllRanges();
      selection.addRange(range);
    } else {
      const range = document.createRange();
      range.selectNodeContents(this._root);
      range.collapse(true);
      selection.removeAllRanges();
      selection.addRange(range);
    }
    if (!skipRelocate) this._relocateIfOffscreen(selection);
    return selection;
  }

  /** Re-place the caret at visible content when it is outside the viewport. */
  _relocateIfOffscreen(selection) {
    const focusRect = this._selectionFocusRect(selection);
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

  /** Count characters in navigable text nodes before (node, offset).
   *  Hidden text (display:none ancestors) is excluded so offsets stay stable. */
  _caretGlobalOffset(node, offset) {
    if (node?.nodeType !== Node.TEXT_NODE || !this._isNavigableTextNode(node)
        || !this._hasRenderedBox(node)) return null;
    const walker = document.createTreeWalker(this._root, NodeFilter.SHOW_TEXT);
    let total = 0;
    while (walker.nextNode()) {
      const textNode = walker.currentNode;
      if (textNode === node)
        return total + Math.min(Math.max(0, offset), textNode.length);
      if (this._isNavigableTextNode(textNode) && this._hasRenderedBox(textNode))
        total += textNode.length;
    }
    return null;
  }

  /** Resolve a global character offset back to a { node, offset }, or null. */
  _resolveGlobalOffset(target) {
    if (!Number.isSafeInteger(target) || target < 0) return null;
    const walker = document.createTreeWalker(this._root, NodeFilter.SHOW_TEXT);
    let total = 0;
    while (walker.nextNode()) {
      const textNode = walker.currentNode;
      if (!this._isNavigableTextNode(textNode) || !this._hasRenderedBox(textNode))
        continue;
      if (total + textNode.length >= target) return { node: textNode, offset: target - total };
      total += textNode.length;
    }
    return null;
  }

  _saveCaretPosition() {
    const selection = window.getSelection();
    const point = this.markActive && this._savedFocus
      ? this._savedFocus
      : selection?.focusNode ? { node: selection.focusNode, offset: selection.focusOffset } : null;
    const bookmark = this._captureTextBookmark(point);
    if (!bookmark) return;
    try {
      localStorage.setItem(this._caretStorageKey(), JSON.stringify({
        offset: bookmark.globalOffset,
        nextBoundary: bookmark.offset === 0
      }));
    } catch (e) { }
    if (location.href === this._historyLocation)
      this._historyPositions.set(location.href, bookmark);
  }

  _applyRange(selection, range) {
    const mark = this.markActive ? this._markAnchorPoint() : null;
    if (mark) {
      selection.setBaseAndExtent(mark.node, mark.offset,
        range.startContainer, range.startOffset);
    } else {
      selection.removeAllRanges();
      selection.addRange(range);
    }
  }

  /** Apply mark selection from anchor to current focus.
   *  When extend is true, shifts the forward endpoint by +1 char so the
   *  character at cursor position is included (needed for end-of-line).
   *  Returns the real (pre-extension) focus for saving to _savedFocus. */
  _applyMarkSelection(selection, anchorNode, anchorOffset, extend = false) {
    const focusNode = selection.focusNode;
    const focusOffset = selection.focusOffset;
    if (extend) {
      const forward = anchorNode === focusNode
        ? anchorOffset < focusOffset
        : !!(anchorNode.compareDocumentPosition(focusNode) & Node.DOCUMENT_POSITION_FOLLOWING);
      if (forward && focusNode.nodeType === Node.TEXT_NODE) {
        selection.setBaseAndExtent(anchorNode, anchorOffset, focusNode,
          Math.min(focusOffset + 1, focusNode.textContent.length));
      } else {
        selection.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
      }
    } else {
      selection.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
    }
    return { node: focusNode, offset: focusOffset };
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
    const selection = window.getSelection();
    if (selection?.focusNode) {
      const el = selection.focusNode.nodeType === Node.ELEMENT_NODE
        ? selection.focusNode : selection.focusNode.parentElement;
      const page = el?.closest('.page[data-page-number]');
      if (page) return page;
    }
    // Fallback: element at viewport center
    const viewport = this._viewportRect();
    const probeX = (viewport.left + viewport.right) / 2;
    const probeY = (viewport.top + viewport.bottom) / 2;
    return document.elementFromPoint(probeX, probeY)?.closest('.page[data-page-number]') || null;
  }

  _visuallyAdjacentPage(currentPage, forward) {
    const pages = Array.from(this._root.querySelectorAll('.page[data-page-number]'));
    if (pages.length < 2) return null;
    pages.sort((firstPage, secondPage) => {
      const firstRect = firstPage.getBoundingClientRect();
      const secondRect = secondPage.getBoundingClientRect();
      const deltaY = firstRect.top - secondRect.top;
      const sameRow = this._isSameLine(firstRect, secondRect);
      return sameRow ? firstRect.left - secondRect.left : deltaY;
    });
    const currentIndex = pages.indexOf(currentPage);
    if (currentIndex === -1) return null;
    const adjacentIndex = forward ? currentIndex + 1 : currentIndex - 1;
    return adjacentIndex >= 0 && adjacentIndex < pages.length ? pages[adjacentIndex] : null;
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
    const viewportTop = viewport.top - margin;
    const viewportBottom = viewport.bottom + margin;
    for (let nodeIndex = 0; nodeIndex < nodes.length; nodeIndex++) {
      const segment = nodes[nodeIndex];
      if (segment.rect.bottom < viewportTop || segment.rect.top > viewportBottom) continue;
      const fragments = this._splitNodeFragments(
        segment.node, segment.domIndex, segment.groupRoot);
      if (!fragments.length) continue;
      nodes.splice(nodeIndex, 1, ...fragments);
      nodeIndex += fragments.length - 1;
    }

    // Cluster into vertical line-bands before sorting so inline boxes that
    // share a visual line but differ in height (e.g. taller <code> spans)
    // sort together by X.
    const byMiddle = [...nodes].sort((firstEntry, secondEntry) =>
      (firstEntry.rect.top + firstEntry.rect.height / 2)
      - (secondEntry.rect.top + secondEntry.rect.height / 2));
    let band = -1, bandMiddle = NaN, bandHeight = 0;
    for (const entry of byMiddle) {
      const middle = entry.rect.top + entry.rect.height / 2;
      if (band < 0 || Math.abs(middle - bandMiddle)
          > Math.max(entry.rect.height, bandHeight) / 2) {
        band++;
        bandMiddle = middle;
        bandHeight = entry.rect.height;
      }
      entry._yBand = band;
    }
    nodes.sort((firstEntry, secondEntry) =>
      (firstEntry._yBand - secondEntry._yBand) ||
      (firstEntry.rect.left - secondEntry.rect.left) ||
      (firstEntry.domIndex - secondEntry.domIndex));

    const lines = this._groupIntoLines(nodes);
    const ordered = [];
    for (const line of lines) {
      line.sort((firstEntry, secondEntry) =>
        (firstEntry.rect.left - secondEntry.rect.left) ||
        (firstEntry.domIndex - secondEntry.domIndex));
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
          const rect = this._lineMoveRect(textNode, mid);
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
    const caretMiddleY = caretRect.top + caretRect.height / 2;
    let bestIndex = -1, bestDistance = Infinity;
    for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
      const firstRect = lines[lineIndex][0].rect;
      const lineMiddleY = firstRect.top + firstRect.height / 2;
      const distance = Math.abs(caretMiddleY - lineMiddleY);
      if (distance < bestDistance) {
        bestDistance = distance;
        bestIndex = lineIndex;
      }
    }
    if (bestIndex < 0 || !this._isSameLine(caretRect, lines[bestIndex][0].rect)) return -1;
    // Among lines at the same Y, prefer the one containing the caret's X.
    // Reuse _isSameLine for Y-proximity (no new thresholds).
    const caretX = caretRect.left + caretRect.width / 2;
    const bestRect = lines[bestIndex][0].rect;
    for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
      const firstRect = lines[lineIndex][0].rect;
      if (!this._isSameLine(firstRect, bestRect)) continue;
      const bounds = this._lineBounds(lines[lineIndex]);
      if (bounds && caretX >= bounds.left && caretX <= bounds.right) return lineIndex;
    }
    return bestIndex;
  }

  _pickPositionOnLine(line, goalX) {
    const lineRect = line[0]?.rect;
    if (!lineRect) return null;
    let bestRange = null, bestDistance = Infinity;
    let bestWhitespaceRange = null, bestWhitespaceDistance = Infinity;
    for (const entry of line) {
      const startOffset = entry.startOffset ?? 0;
      const endOffset = entry.endOffset ?? entry.node.length;
      for (let characterOffset = startOffset; characterOffset < endOffset; characterOffset++) {
        const range = document.createRange();
        range.setStart(entry.node, characterOffset);
        range.setEnd(entry.node, characterOffset + 1);
        const rect = range.getBoundingClientRect();
        if (!rect.width || !rect.height) continue;
        if (!this._isSameLine(rect, lineRect)) continue;
        const distance = Math.abs(rect.left + rect.width / 2 - goalX);
        // Prefer a visible character so the caret doesn't land on invisible
        // indentation whitespace.
        if (/\s/.test(entry.node.textContent[characterOffset])) {
          if (distance < bestWhitespaceDistance) {
            bestWhitespaceDistance = distance;
            bestWhitespaceRange = this._collapsedRange(entry.node, characterOffset);
          }
        } else if (distance < bestDistance) {
          bestDistance = distance;
          bestRange = this._collapsedRange(entry.node, characterOffset);
        }
      }
    }
    return bestRange || bestWhitespaceRange;
  }

  /** Find the segment containing the caret offset in the visual ordering.
   *  Returns { ordered, segmentIndex } or null. Shared by char/word movement. */
  _findCaretSegment(focusNode, focusOffset, scopeRoot) {
    const { ordered } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;
    for (let segmentIndex = 0; segmentIndex < ordered.length; segmentIndex++) {
      const entry = ordered[segmentIndex];
      if (entry.node === focusNode && focusOffset >= entry.startOffset
          && focusOffset <= entry.endOffset) {
        return { ordered, segmentIndex };
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



  _moveCharVisual(selection, forward) {
    const focusNode = selection.focusNode;
    const focusOffset = selection.focusOffset;
    if (focusNode.nodeType !== Node.TEXT_NODE) return false;
    const text = focusNode.textContent;

    if (forward) {
      if (focusOffset < focusNode.length) {
        let step = 1;
        if (text.charCodeAt(focusOffset) >= 0xD800 && text.charCodeAt(focusOffset) <= 0xDBFF
          && focusOffset + 1 < focusNode.length
          && text.charCodeAt(focusOffset + 1) >= 0xDC00
          && text.charCodeAt(focusOffset + 1) <= 0xDFFF) {
          step = 2;
        }
        selection.collapse(focusNode, focusOffset + step);
        return true;
      }
      const scopeRoot = this._pageScopeRoot(focusNode);
      const result = this._findCaretSegment(focusNode, focusOffset, scopeRoot);
      if (!result) return false;
      const { ordered, segmentIndex } = result;
      let targetSegmentIndex = segmentIndex + 1;
      while (targetSegmentIndex < ordered.length
          && ordered[targetSegmentIndex].node === focusNode) targetSegmentIndex++;
      if (targetSegmentIndex >= ordered.length) return false;
      selection.collapse(ordered[targetSegmentIndex].node, 0);
      return true;
    }

    if (focusOffset > 0) {
      let step = 1;
      if (text.charCodeAt(focusOffset - 1) >= 0xDC00
        && text.charCodeAt(focusOffset - 1) <= 0xDFFF
        && focusOffset - 2 >= 0
        && text.charCodeAt(focusOffset - 2) >= 0xD800
        && text.charCodeAt(focusOffset - 2) <= 0xDBFF) {
        step = 2;
      }
      selection.collapse(focusNode, focusOffset - step);
      return true;
    }
    const scopeRoot = this._pageScopeRoot(focusNode);
    const result = this._findCaretSegment(focusNode, focusOffset, scopeRoot);
    if (!result) return false;
    const { ordered, segmentIndex } = result;
    let targetSegmentIndex = segmentIndex - 1;
    while (targetSegmentIndex >= 0
        && ordered[targetSegmentIndex].node === focusNode) targetSegmentIndex--;
    if (targetSegmentIndex < 0) return false;
    selection.collapse(ordered[targetSegmentIndex].node,
      ordered[targetSegmentIndex].endOffset);
    return true;
  }

  /** Visual word movement (handles CJK, multi-segment nodes, Unicode word chars). */
  _moveWordVisual(selection, forward) {
    const focusNode = selection.focusNode;
    const focusOffset = selection.focusOffset;
    if (focusNode.nodeType !== Node.TEXT_NODE) return false;

    const scopeRoot = this._pageScopeRoot(focusNode);
    const result = this._findCaretSegment(focusNode, focusOffset, scopeRoot);
    if (!result) return false;
    const { ordered, segmentIndex: currentSegmentIndex } = result;

    if (forward) {
      let node = focusNode;
      let offset = focusOffset;
      let segmentIndex = currentSegmentIndex;
      while (offset < node.textContent.length && this._isWordChar(node.textContent[offset]))
        offset++;
      while (true) {
        while (offset < node.textContent.length && !this._isWordChar(node.textContent[offset]))
          offset++;
        if (offset < node.textContent.length) {
          selection.collapse(node, offset);
          return true;
        }
        do { segmentIndex++; }
        while (segmentIndex < ordered.length && ordered[segmentIndex].node === node);
        if (segmentIndex >= ordered.length) return false;
        node = ordered[segmentIndex].node;
        offset = 0;
      }
    }

    let node = focusNode;
    let offset = focusOffset;
    let segmentIndex = currentSegmentIndex;
    if (offset > 0) {
      offset--;
    } else {
      do { segmentIndex--; }
      while (segmentIndex >= 0 && ordered[segmentIndex].node === node);
      if (segmentIndex < 0) return false;
      node = ordered[segmentIndex].node;
      offset = node.textContent.length - 1;
    }
    while (true) {
      while (offset >= 0 && !this._isWordChar(node.textContent[offset])) offset--;
      if (offset >= 0) break;
      do { segmentIndex--; }
      while (segmentIndex >= 0 && ordered[segmentIndex].node === node);
      if (segmentIndex < 0) return false;
      node = ordered[segmentIndex].node;
      offset = node.textContent.length - 1;
    }
    while (offset > 0 && this._isWordChar(node.textContent[offset - 1])) offset--;
    selection.collapse(node, offset);
    return true;
  }

  /** Snap the selection focus onto a visible text node. */
  _snapToText(selection, forward) {
    const focusNode = selection.focusNode;
    if (focusNode.nodeType === Node.TEXT_NODE && this._isNavigableTextNode(focusNode)) return;

    const pastEnd = focusNode.nodeType === Node.ELEMENT_NODE
      && forward && selection.focusOffset >= focusNode.childNodes.length;
    const preferForward = pastEnd ? false
      : focusNode.nodeType === Node.ELEMENT_NODE ? forward : !forward;

    const textNode = this._walkToVisible(focusNode, preferForward)
      || this._walkToVisible(focusNode, !preferForward);
    if (textNode) {
      selection.collapse(textNode, this._textVisibleEdgeOffset(textNode, !forward));
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
    const currentRect = lines[currentLineIndex][0].rect;
    const start = isForward ? currentLineIndex + 1 : currentLineIndex - 1;
    const end = isForward ? lines.length : -1;
    for (let i = start; isForward ? i < end : i > end; i += isForward ? 1 : -1) {
      const targetRect = lines[i][0].rect;
      if (this._isSameLine(targetRect, currentRect)) continue;
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

  /** Move by logical line/column within a single preserved-whitespace text node. */
  _moveWithinPreservedTextNode(node, offset, forward) {
    if (this._isPdfMode() || node?.nodeType !== Node.TEXT_NODE) return null;
    if (!this._preservesWhitespace(node)) return null;
    const text = node.textContent || "";
    if (!text.includes('\n')) return null;

    const length = text.length;
    const clampedOffset = Math.max(0, Math.min(offset, length));
    const currentStart = text.lastIndexOf('\n', Math.max(0, clampedOffset - 1)) + 1;
    let currentEnd = text.indexOf('\n', currentStart);
    if (currentEnd < 0) currentEnd = length;
    const currentOffset = Math.max(currentStart, Math.min(clampedOffset, currentEnd));
    const column = currentOffset - currentStart;

    let targetStart = -1;
    let targetEnd = -1;
    if (forward) {
      if (currentEnd >= length) return null;
      targetStart = currentEnd + 1;
      targetEnd = text.indexOf('\n', targetStart);
      if (targetEnd < 0) targetEnd = length;
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
      if (forward) {
        if (targetEnd >= length) return null;
        targetStart = targetEnd + 1;
        targetEnd = text.indexOf('\n', targetStart);
        if (targetEnd < 0) targetEnd = length;
      } else {
        if (targetStart <= 0) return null;
        targetEnd = targetStart - 1;
        const prevBreak = text.lastIndexOf('\n', Math.max(0, targetEnd - 1));
        targetStart = prevBreak + 1;
      }
    }
    return null;
  }

  /** DOM-based visual line movement with explicit outcome kinds. */
  _moveLine(forward, cachedGoalX = null, allowScroll = true) {
    const selection = window.getSelection();
    if (!selection?.rangeCount) return { kind: 'boundary' };

    // Use the actual focus when it has a usable rect — whitespace-only
    // nodes are real line positions; resolving off them lands on a neighbor
    // line and makes moves look stuck.
    let lineNode = selection.focusNode, lineOffset = selection.focusOffset;
    let caretRect = this._lineMoveRect(lineNode, lineOffset);
    if (!caretRect?.height) {
      const resolved = this._resolveCursorPosition(selection.focusNode, selection.focusOffset, forward);
      lineNode = resolved.node; lineOffset = resolved.offset;
      const startRange = this._collapsedRange(lineNode, lineOffset);
      caretRect = this._lineMoveRect(lineNode, lineOffset) || this._charRect(startRange);
    }
    if (!caretRect?.height) return { kind: 'boundary' };
    const goalX = cachedGoalX !== null ? cachedGoalX : (caretRect.left + caretRect.width / 2);

    const preservedTextRange = this._moveWithinPreservedTextNode(lineNode, lineOffset, forward);
    if (preservedTextRange) {
      return { kind: 'moved', range: preservedTextRange, gapJump: false };
    }

    // Phase 1: move within current scope (PDF page or full HTML body)
    const currentPage = this._currentPage();
    const scopeRoot = currentPage || this._root;
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length || !lines.length) return { kind: 'boundary' };

    let currentLineIndex = this._findCaretLine(lines, caretRect);
    let forceGapJump = false;
    if (currentLineIndex < 0) {
      const midY = caretRect.top + caretRect.height / 2;
      let bestDistance = Infinity;
      for (let i = 0; i < lines.length; i++) {
        const distance = Math.abs(lines[i][0].rect.top + lines[i][0].rect.height / 2 - midY);
        if (distance < bestDistance) {
          bestDistance = distance;
          currentLineIndex = i;
        }
      }
    }
    if (currentLineIndex < 0) return { kind: 'boundary' };
    const currentBounds = this._lineBounds(lines[currentLineIndex]);
    let targetLineIndex = this._lineTargetIndex(currentLineIndex, forward, lines);

    if (targetLineIndex >= 0) {
      const currentRect = lines[currentLineIndex][0].rect;
      const targetRect = lines[targetLineIndex][0].rect;
      const currentBottom = currentRect.bottom ?? currentRect.top + currentRect.height;
      const targetBottom = targetRect.bottom ?? targetRect.top + targetRect.height;
      const lineGap = forward ? targetRect.top - currentBottom : currentRect.top - targetBottom;
      const viewport = this._viewportRect();
      const targetInViewport = targetRect.top < viewport.bottom && targetBottom > viewport.top;
      const targetBounds = this._lineBounds(lines[targetLineIndex]);
      const crossColumn = !targetInViewport && currentBounds && targetBounds
        && (currentBounds.right < targetBounds.left || targetBounds.right < currentBounds.left);
      if ((lineGap > this._scrollPx || crossColumn) && !targetInViewport) {
        if (!allowScroll) return { kind: 'stepped' };
        if (this._canScroll(forward)) {
          const maximum = Math.min(this._scrollPx, Math.max(1, viewport.bottom - viewport.top));
          const distance = forward
            ? Math.max(1, targetBottom - viewport.bottom)
            : Math.max(1, viewport.top - targetRect.top);
          const step = Math.min(maximum, distance);
          this._suppressScrollRelocate = true;
          const moved = this._scrollBy(forward ? step : -step);
          if (!moved) {
            this._suppressScrollRelocate = false;
            forceGapJump = true;
          } else if (step < distance) {
            return { kind: 'stepped' };
          } else {
            return { kind: 'retry', goalX };
          }
        } else {
          forceGapJump = true;
        }
      }
    }

    // Target line found — merge adjacent line groups and pick position
    if (targetLineIndex >= 0) {
      let picked = null;
      // A line whose characters yield no rects (detached by a re-render,
      // culled animation frame) is unlandable — skip to the next candidate.
      while (targetLineIndex >= 0 && targetLineIndex < lines.length) {
        // Merge entries from adjacent line groups at the same visual Y,
        // but only from the same column (horizontally overlapping with caret)
        const mergedLine = [...lines[targetLineIndex]];
        const targetLineRect = lines[targetLineIndex][0].rect;
        for (let j = targetLineIndex - 1; j >= 0; j--) {
          if (!this._isSameLine(lines[j][0].rect, targetLineRect)) break;
          const bounds = this._lineBounds(lines[j]);
          if (currentBounds && bounds && (currentBounds.right < bounds.left || bounds.right < currentBounds.left)) continue;
          mergedLine.unshift(...lines[j]);
        }
        for (let j = targetLineIndex + 1; j < lines.length; j++) {
          if (!this._isSameLine(lines[j][0].rect, targetLineRect)) break;
          const bounds = this._lineBounds(lines[j]);
          if (currentBounds && bounds && (currentBounds.right < bounds.left || bounds.right < currentBounds.left)) continue;
          mergedLine.push(...lines[j]);
        }
        picked = this._pickPositionOnLine(mergedLine, goalX);
        if (picked) break;
        targetLineIndex = this._lineTargetIndex(targetLineIndex, forward, lines);
      }
      if (picked) {
        return { kind: 'moved', range: picked, gapJump: forceGapJump };
      }
      // Every candidate below/above is unlandable — fall through to the
      // incremental-scroll / cross-page handling as if no target existed.
    }

    // No target line in scope — scroll incrementally or cross page
    if (currentPage) {
      if (this._canScroll(forward)) {
        this._suppressScrollRelocate = true;
        const moved = this._scrollBy(forward ? this._scrollPx : -this._scrollPx);
        if (!moved) {
          this._suppressScrollRelocate = false;
          return { kind: 'boundary' };
        }
        // Check if adjacent page is now near viewport edge
        const adjacentPage = this._visuallyAdjacentPage(currentPage, forward);
        if (adjacentPage) {
          const pageRect = adjacentPage.getBoundingClientRect();
          const viewport = this._viewportRect();
          const pageNearEdge = forward
            ? pageRect.top < viewport.top + this._scrollPx
            : pageRect.bottom > viewport.bottom - this._scrollPx;
          if (pageNearEdge) {
            // Adjacent page is close - cross to it
            return { kind: 'moved',
              range: this._moveLineCrossPage(currentPage, forward, goalX), gapJump: false };
          }
        }
        // Scrolled but page not near yet - stop here (don't retry)
        return { kind: 'boundary' };
      }
      // Can't scroll - cross to adjacent page
      return { kind: 'moved',
        range: this._moveLineCrossPage(currentPage, forward, goalX), gapJump: false };
    }
    if (!allowScroll) return { kind: 'boundary' };
    return this._scrollForLineRetry(forward, goalX);
  }

  /** Scroll toward off-screen text and request one fresh-model retry. */
  _scrollForLineRetry(forward, goalX) {
    if (!this._canScroll(forward)) return { kind: 'boundary' };
    this._suppressScrollRelocate = true;
    const moved = this._scrollBy(forward ? this._scrollPx : -this._scrollPx);
    if (!moved) this._suppressScrollRelocate = false;
    return moved ? { kind: 'retry', goalX } : { kind: 'boundary' };
  }

  /** Cross PDF page boundary for line movement. Returns a collapsed Range or null. */
  _moveLineCrossPage(page, forward, goalX) {
    if (!page) return null;
    let adjacentPage = page;
    const maximumSkips = this._root.querySelectorAll('.page[data-page-number]').length;
    let skipped = 0;
    while ((adjacentPage = this._visuallyAdjacentPage(adjacentPage, forward))
        && skipped < maximumSkips) {
      const { ordered: adjacentOrdered, lines } = this._visuallyOrderedTextNodes(adjacentPage);
      if (!adjacentOrdered.length) { skipped++; continue; }
      // Found text — scroll into view if needed, then pick line
      const adjacentRect = adjacentPage.getBoundingClientRect();
      const viewportRect = this._viewportRect();
      let adjacentLines = lines;
      if (adjacentRect.bottom < viewportRect.top || adjacentRect.top > viewportRect.bottom) {
        adjacentPage.scrollIntoView({ block: forward ? 'start' : 'end', behavior: 'instant' });
        this._invalidateLayoutCaches();
        // The scroll shifted every client rect — re-read the lines before
        // picking so live character rects meet fresh segment rects.
        adjacentLines = this._visuallyOrderedTextNodes(adjacentPage).lines;
      }
      if (!adjacentLines.length) { skipped++; continue; }
      const targetLine = forward ? adjacentLines[0] : adjacentLines[adjacentLines.length - 1];
      return this._pickPositionOnLine(targetLine, goalX);
    }
    // Scroll first adjacent page into view to trigger text layer loading
    const firstAdjacentPage = this._visuallyAdjacentPage(page, forward);
    if (firstAdjacentPage) {
      firstAdjacentPage.scrollIntoView({ block: forward ? 'start' : 'end', behavior: 'instant' });
      this._invalidateLayoutCaches();
    }
    return null;
  }

  /* ── Movement Dispatcher ───────────────────────────────────── */

  _moveCaret(direction, granularity) {
    this._cancelCaretTask();
    const selection = this._ensureSelection(true);
    if (!selection) { return false; }

    // Capture mark anchor before any collapse loses it — _markAnchor is the
    // source of truth; _markAnchorPoint clamps a stale offset.
    const markPoint = this.markActive ? this._markAnchorPoint() : null;
    const markAnchorNode = markPoint ? markPoint.node : (this.markActive ? selection.anchorNode : null);
    const markAnchorOffset = markPoint ? markPoint.offset : (this.markActive ? selection.anchorOffset : null);

    // Restore the real cursor (pre-extension) so movement starts from the
    // actual caret.
    const focusPoint = this._savedFocusPoint();
    if (focusPoint && this.markActive && selection.rangeCount) {
      selection.collapse(focusPoint.node, focusPoint.offset);
    }

    this._hitBoundary = false;
    if (this._isAtVisibleBoundary(direction)) {
      this._hitBoundary = true;
      return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
    }

    // Pre-snap whitespace focus to visible text. Skip for line/
    // lineboundary: they use the visual-line model and a backward snap here
    // would cancel a forward line move.
    const skipPreSnap = granularity === "line" || granularity === "lineboundary";
    if (!skipPreSnap) {
      const { node: snapNode, offset: snapOffset } =
        this._resolveCursorPosition(selection.focusNode, selection.focusOffset);
      if (snapNode !== selection.focusNode || snapOffset !== selection.focusOffset) {
        if (this.markActive) {
          selection.setBaseAndExtent(markAnchorNode, markAnchorOffset, snapNode, snapOffset);
        } else {
          selection.collapse(snapNode, snapOffset);
        }
      }
    }

    if (!this.markActive && !selection.isCollapsed) {
      selection.collapse(selection.focusNode, selection.focusOffset);
    }

    const forward = direction === "forward";
    const startNode = selection.focusNode, startOffset = selection.focusOffset;
    if (this.markActive) { selection.collapse(selection.focusNode, selection.focusOffset); }

    // PDF text layers and HTML both use visual ordering for char/word movement.
    if (granularity === "character" || granularity === "word") {
      let moved = granularity === "character"
        ? this._moveCharVisual(selection, forward)
        : this._moveWordVisual(selection, forward);
      // After expandSelection, _moveWordVisual lands at the start of the
      // next word — extend past it so the word is included.
      if (moved && this._savedCaret && granularity === "word") {
        let node = selection.focusNode, offset = selection.focusOffset;
        if (node.nodeType === Node.TEXT_NODE) {
          if (forward) {
            while (offset < node.textContent.length && this._isWordChar(node.textContent[offset])) offset++;
          } else {
            while (offset > 0 && this._isWordChar(node.textContent[offset - 1])) offset--;
          }
          if (offset !== selection.focusOffset) selection.collapse(node, offset);
        }
      }
      if (moved) this._snapToText(selection, forward);
      if (moved && this._movedWrongWay(startNode, startOffset, selection.focusNode, selection.focusOffset, forward)) {
        selection.collapse(startNode, startOffset);
        moved = false;
      }
      return this._finishMove(selection, moved, markAnchorNode, markAnchorOffset);
    }

    // Line: unified visual line movement for both PDF and HTML.
    if (granularity === "line") {
      let previousCaretRect = this._rangeRectAt(startNode, startOffset)
        || this._collapsedRange(startNode, startOffset).getBoundingClientRect();

      // A final bounded scroll invalidates line indexes. Rebuild once and
      // recompute the adjacent target, preserving only the horizontal goal.
      let result = this._moveLine(forward);
      if (result.kind === 'stepped') {
        if (this.markActive && markAnchorNode != null)
          this._applyMarkSelection(selection, markAnchorNode, markAnchorOffset);
        return true;
      }
      if (result.kind === 'retry') {
        previousCaretRect = this._rangeRectAt(startNode, startOffset)
          || this._collapsedRange(startNode, startOffset).getBoundingClientRect();
        result = this._moveLine(forward, result.goalX, false);
        if (result.kind === 'retry' || result.kind === 'stepped'
            || result.kind === 'boundary') {
          this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
          return true;
        }
      }

      let lineRange = result.kind === 'moved' ? result.range : null;
      // _moveLine may stay on the caret's own row (same-Y cluster); if so,
      // probe one line over.
      const movedRect = lineRange && this._rangeRectAt(lineRange.startContainer, lineRange.startOffset);
      if (lineRange && movedRect && previousCaretRect && this._isSameLine(movedRect, previousCaretRect)) {
        const probe = this._probeLineFromCaret(forward, previousCaretRect);
        if (probe) lineRange = probe;
      }
      if (!lineRange) {
        const atVisibleBoundary = this._isAtVisibleBoundary(direction);
        const atViewportEdge = forward ? this.isAtBottom() : this.isAtTop();
        this._hitBoundary = atVisibleBoundary || atViewportEdge;
        return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      }

      selection.removeAllRanges();
      selection.addRange(lineRange);

      // If caret was off-screen before move, reveal it. A gap jump needs a
      // full reveal — the bounded scroll would leave the target off screen.
      const vpNow = this._viewportRect();
      const preBottom = previousCaretRect ? (previousCaretRect.bottom ?? (previousCaretRect.top + previousCaretRect.height)) : null;
      const preOnscreen = previousCaretRect && preBottom >= vpNow.top && previousCaretRect.top <= vpNow.bottom;
      if (!preOnscreen) {
        this._savedFocus = this.markActive && markAnchorNode != null
          ? this._applyMarkSelection(selection, markAnchorNode, markAnchorOffset)
          : { node: selection.focusNode, offset: selection.focusOffset };
        if (result.kind === 'moved' && result.gapJump) this._scrollToSelection();
        else this._scrollToSelectionLineBounded();
        this._updateCursor();
        return true;
      }

      return this._finishMove(selection, true, markAnchorNode, markAnchorOffset);
    }

    // Lineboundary: selection.modify is unreliable in xwidget (goes to text node
    // boundary); use the visual line model.
    if (granularity === "lineboundary") {
      const scopeRoot = (this._isPdfMode() ? this._pageScopeRoot(selection.focusNode) : null) || this._root;
      const { lines } = this._visuallyOrderedTextNodes(scopeRoot);
      if (!lines.length) return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const caretRect = this._rangeRectAt(selection.focusNode, selection.focusOffset)
        || this._collapsedRange(selection.focusNode, selection.focusOffset).getBoundingClientRect();
      if (!caretRect?.height) return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const lineIndex = this._findCaretLine(lines, caretRect);
      if (lineIndex < 0) return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const line = lines[lineIndex];
      // Find the first/last character on this line that has a valid rect.
      let targetRange = null;
      if (forward) {
        // End of line: scan backwards from last segment's end
        for (let segmentIndex = line.length - 1; segmentIndex >= 0 && !targetRange; segmentIndex--) {
          const segment = line[segmentIndex];
          for (let characterOffset = segment.endOffset - 1;
            characterOffset >= segment.startOffset; characterOffset--) {
            const characterRect = this._rangeRectAt(segment.node, characterOffset);
            if (characterRect) {
              targetRange = this._collapsedRange(segment.node, characterOffset);
              break;
            }
          }
        }
      } else {
        // Beginning of line: scan forwards from first segment's start
        for (let segmentIndex = 0; segmentIndex < line.length && !targetRange; segmentIndex++) {
          const segment = line[segmentIndex];
          for (let characterOffset = segment.startOffset;
            characterOffset < segment.endOffset; characterOffset++) {
            const characterRect = this._rangeRectAt(segment.node, characterOffset);
            if (characterRect) {
              targetRange = this._collapsedRange(segment.node, characterOffset);
              break;
            }
          }
        }
      }
      if (targetRange) {
        selection.removeAllRanges();
        selection.addRange(targetRange);
      }
      return this._finishMove(selection, !!targetRange, markAnchorNode, markAnchorOffset,
        !!targetRange && forward);
    }

    // PDF sentence: use visual line model to find [.!?。！？] boundaries.
    if (this._isPdfMode() && granularity === "sentence") {
      const { node, offset } = this._resolveCursorPosition(selection.focusNode, selection.focusOffset);
      if (node.nodeType !== Node.TEXT_NODE || !this._isContained(node))
        return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const scopeRoot = this._pageScopeRoot(node);
      const { lines } = this._visuallyOrderedTextNodes(scopeRoot);
      if (!lines.length) return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const caretRange = this._collapsedRange(node, offset);
      const caretRect = this._charRect(caretRange) || caretRange.getBoundingClientRect();
      if (!caretRect?.height) return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const currentLineIndex = this._findCaretLine(lines, caretRect);
      if (currentLineIndex < 0)
        return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      const caretPoint = { node, offset };
      const columnLeft = this._lineBounds(lines[currentLineIndex])?.left ?? null;
      const model = this._pdfTextRangeModel(lines, 0, lines.length - 1, columnLeft);
      const caretOffset = model.offsetFromPoint(caretPoint.node, caretPoint.offset);
      const probeOff = this._skipSentenceBoundary(model.text, caretOffset, forward);
      const sentenceBounds = this._pdfSentenceOffsets(model.text, probeOff);
      const targetOffset = forward ? sentenceBounds.end : sentenceBounds.start;
      const target = model.pointFromOffset(targetOffset);
      if (!target?.node || this._movedWrongWay(caretPoint.node, caretPoint.offset, target.node, target.offset, forward))
        return this._finishMove(selection, false, markAnchorNode, markAnchorOffset);
      selection.collapse(target.node, target.offset);
      return this._finishMove(selection, true, markAnchorNode, markAnchorOffset);
    }

    // Default (sentence, etc.): selection.modify with guards
    let moved = this._stepModify(selection, direction, granularity, forward);
    this._snapToText(selection, forward);
    if (selection.focusNode === startNode && selection.focusOffset === startOffset) { moved = false; }

    if (moved && this._movedWrongWay(startNode, startOffset, selection.focusNode, selection.focusOffset, forward)) {
      if (this.markActive) selection.setBaseAndExtent(markAnchorNode, markAnchorOffset, startNode, startOffset);
      else selection.collapse(startNode, startOffset);
      moved = false;
    }

    // Fallback: void elements that selection.modify cannot cross
    if (!moved) {
      moved = this._fallbackToAdjacentText(selection, forward, markAnchorNode, markAnchorOffset);
    }

    return this._finishMove(selection, moved, markAnchorNode, markAnchorOffset);
  }

  /** Common epilogue: restore mark, save caret, scroll & redraw.
   *  extend (end-of-line) shifts the endpoint +1 so the char at the cursor
   *  is included; _savedFocus stores the real (pre-extension) offset. */
  _finishMove(selection, moved, anchorNode, anchorOffset, extend = false) {
    if (this.markActive && anchorNode != null) {
      this._savedFocus = this._applyMarkSelection(selection, anchorNode, anchorOffset, extend);
    }
    if (moved) {
      if (!this.markActive || anchorNode == null) {
        this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
      }
      this._scrollToSelection();
      this._updateCursor();
    }
    return moved;
  }

  _stepModify(selection, direction, granularity, forward) {
    const step = () => {
      const { focusNode: previousNode, focusOffset: previousOffset } = selection;
      selection.modify("move", direction, granularity);
      if (!this._isContained(selection.focusNode)) {
        selection.collapse(previousNode, previousOffset);
        return false;
      }
      if ((selection.focusNode !== previousNode || selection.focusOffset !== previousOffset)
          && this._movedWrongWay(previousNode, previousOffset,
            selection.focusNode, selection.focusOffset, forward)) {
        selection.collapse(previousNode, previousOffset);
        return false;
      }
      this._unstickCaret(selection, direction, previousNode, previousOffset);
      return selection.focusNode !== previousNode || selection.focusOffset !== previousOffset;
    };
    let moved = step();
    while (moved && selection.focusNode.nodeType === Node.ELEMENT_NODE) {
      if (!step()) { break; }
    }
    return moved;
  }

  /** Nudge the caret past elements that selection.modify cannot cross. */
  _unstickCaret(selection, direction, previousNode, previousOffset) {
    const focus = selection.focusNode, forward = direction === "forward";
    // Case 1: inside an empty/void element — hop to parent edge.
    if (focus.nodeType === Node.ELEMENT_NODE && !focus.firstChild) {
      const parent = focus.parentNode;
      if (parent) {
        const childIndex = Array.from(parent.childNodes).indexOf(focus);
        selection.collapse(parent, forward ? childIndex + 1 : childIndex);
      }
      return;
    }
    if (focus !== previousNode || selection.focusOffset !== previousOffset) return;
    // Case 2: stuck at a text-node edge — walk to the next text node.
    if (focus.nodeType === Node.TEXT_NODE) {
      if (forward && previousOffset < focus.length) return;
      if (!forward && previousOffset > 0) return;
      const textNode = this._walkToVisible(focus, forward);
      if (textNode) selection.collapse(textNode, forward ? 0 : textNode.length);
      return;
    }
    // Case 3: stuck at an element position — step over adjacent empty child.
    if (focus.nodeType === Node.ELEMENT_NODE) {
      const adjacentChild = focus.childNodes[
        forward ? previousOffset : previousOffset - 1];
      if (adjacentChild && adjacentChild.nodeType === Node.ELEMENT_NODE
          && !adjacentChild.textContent)
        selection.collapse(focus,
          forward ? previousOffset + 1 : previousOffset - 1);
    }
  }

  /** Jump to the next/prev visible text node when selection.modify fails. */
  _fallbackToAdjacentText(selection, forward, anchorNode = null, anchorOffset = null) {
    const startNode = selection.focusNode.nodeType === Node.TEXT_NODE
      ? selection.focusNode
      : (selection.focusNode.childNodes[selection.focusOffset] || selection.focusNode);
    const textNode = this._walkToVisible(startNode, forward);
    if (!textNode || textNode.nodeType !== Node.TEXT_NODE) return false;
    const offset = this._textVisibleEdgeOffset(textNode, forward);
    if (anchorNode != null)
      selection.setBaseAndExtent(anchorNode, anchorOffset, textNode, offset);
    else selection.collapse(textNode, offset);
    return true;
  }

  /* ── Boundary Detection ────────────────────────────────────── */

  _isAtVisibleBoundary(direction) {
    const selection = window.getSelection();
    if (!selection?.rangeCount) return false;
    const node = selection.focusNode, offset = selection.focusOffset;
    const forward = direction === "forward";
    if (node.nodeType !== Node.TEXT_NODE) return false;
    if (!this._isNavigableTextNode(node)) return !this._walkToVisible(node, forward);
    if (forward ? offset < node.length : offset > 0) return false;
    const hasNextVisible = !!this._walkToVisible(node, forward);
    if (hasNextVisible) return false;
    // No more text in DOM — but in multi-page PDF, adjacent pages may have
    // unloaded text layers; not a true boundary.
    const page = node.parentElement?.closest('.page[data-page-number]');
    const hasAdjPage = !!(page && this._visuallyAdjacentPage(page, forward));
    if (hasAdjPage) return false;
    return true;
  }

  isAtBottom() {
    return Math.ceil(this._scrollTop + this._viewportHeight) >= this._scrollHeight;
  }

  isAtTop() {
    return Math.floor(this._scrollTop) <= 0;
  }

  _canScroll(forward) {
    return forward
      ? Math.ceil(this._scrollTop + this._viewportHeight) < this._scrollHeight
      : Math.floor(this._scrollTop) > 0;
  }

  /* ── PDF Selection & Expansion ─────────────────────────────── */

  _detectSelectionScope() {
    const selection = window.getSelection();
    if (!selection?.rangeCount || selection.isCollapsed) return 'none';
    if (this.scrollContainer) {
      const pdfScope = this._selectionCoversPdfLines(selection);
      if (pdfScope) {
        return pdfScope;
      }
    }
    return /[.!?]|\S\s+\S/.test(selection.toString()) ? 'sentence' : 'word';
  }

  _selectionCoversPdfLines(selection) {
    const range = selection.getRangeAt(0);
    const startNode = range.startContainer;
    const endNode = range.endContainer;
    if (startNode.nodeType !== Node.TEXT_NODE || endNode.nodeType !== Node.TEXT_NODE) return null;

    const scopeRoot = this._pageScopeRoot(startNode);
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length) return null;

    const selectedRange = this._selectedPdfLineRange(selection, lines);
    if (!selectedRange) return null;

    const { start, end } = selectedRange;
    if (start === end && !/\s/.test(selection.toString())) return null;

    const startBounds = this._lineBounds(lines[start]);
    const endBounds = this._lineBounds(lines[end]);
    if (!startBounds || !endBounds) return null;

    const coversWholeLines = range.startContainer === startBounds.first.node
      && range.startOffset === 0
      && range.endContainer === endBounds.last.node
      && range.endOffset === endBounds.last.node.length;
    if (!coversWholeLines) return null;

    const text = selection.toString();
    const trimmedText = text.trim();
    if (/[.!?]/.test(trimmedText) || end > start || /\n/.test(text)) return 'sentence';
    return 'word';
  }

  _selectedPdfLineRange(selection, lines) {
    if (!selection?.rangeCount || selection.isCollapsed) return null;
    const range = selection.getRangeAt(0);
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

  _pdfLineContext(selection) {
    const { node, offset } = this._resolveCursorPosition(selection.anchorNode, selection.anchorOffset);
    if (node.nodeType !== Node.TEXT_NODE || !this._isContained(node)) return null;

    const scopeRoot = this._pageScopeRoot(node);
    const { ordered, lines } = this._visuallyOrderedTextNodes(scopeRoot);
    if (!ordered.length || !lines.length) return null;

    const caretRange = this._collapsedRange(node, offset);
    const caretRect = this._charRect(caretRange) || caretRange.getBoundingClientRect();
    if (!caretRect?.height) return null;

    const currentLineIndex = this._findCaretLine(lines, caretRect);
    return currentLineIndex < 0 ? null : { lines, currentLineIndex };
  }

  _isSameColumn(entries, columnLeft) {
    if (columnLeft == null) return true;
    const lineLeft = entries.reduce((minimum, entry) =>
      entry.rect?.height ? Math.min(minimum, entry.rect.left) : minimum, Infinity);
    if (lineLeft === Infinity) return false;
    const lineHeight = this._lineBounds(entries)?.height;
    return lineHeight > 0 && Math.abs(lineLeft - columnLeft) <= lineHeight * 2;
  }

  _pdfTextRangeModel(lines, startLineIndex, endLineIndex, columnLeft = null) {
    const blockLines = [];
    for (let lineIndex = startLineIndex; lineIndex <= endLineIndex; lineIndex++) {
      let entries = lines[lineIndex] || [];
      if (!this._isSameColumn(entries, columnLeft)) continue;
      entries = entries.filter(entry => entry.rect?.height);
      if (entries.length > 0) {
        blockLines.push({ index: lineIndex, entries });
      }
    }
    // Build text with paragraph-aware separators (\n\n for paragraph breaks)
    const lineTexts = blockLines.map(({ entries }) =>
      entries.map((entry) => entry.node.textContent || '').join('')
    );
    const separatorLengths = [];
    let text = lineTexts[0] || '';
    for (let lineIndex = 0; lineIndex < blockLines.length - 1; lineIndex++) {
      const currentTop = blockLines[lineIndex].entries[0]?.rect?.top ?? 0;
      const nextTop = blockLines[lineIndex + 1]?.entries[0]?.rect?.top ?? 0;
      const lineHeight = blockLines[lineIndex].entries[0]?.rect?.height;
      const gap = nextTop - currentTop;
      const paragraphBreak = lineHeight > 0 && gap > lineHeight + lineHeight / 2;
      separatorLengths.push(paragraphBreak ? 2 : 1);
      text += (paragraphBreak ? '\n\n' : '\n') + (lineTexts[lineIndex + 1] || '');
    }

    return {
      text,
      offsetFromPoint: (node, offset) => {
        let total = 0;
        for (let lineIndex = 0; lineIndex < blockLines.length; lineIndex++) {
          const { entries } = blockLines[lineIndex];
          for (const entry of entries) {
            if (entry.node === node) {
              return total + Math.min(offset, entry.node.length);
            }
            total += entry.node.length;
          }
          if (lineIndex < blockLines.length - 1)
            total += separatorLengths[lineIndex];
        }
        return total;
      },
      pointFromOffset: (offset) => {
        let remaining = Math.max(0, Math.min(offset, text.length));
        for (let lineIndex = 0; lineIndex < blockLines.length; lineIndex++) {
          const { index, entries } = blockLines[lineIndex];
          for (const entry of entries) {
            if (remaining <= entry.node.length) {
              return { node: entry.node, offset: remaining, line: index };
            }
            remaining -= entry.node.length;
          }
          if (lineIndex < blockLines.length - 1) {
            const separatorLength = separatorLengths[lineIndex];
            if (remaining < separatorLength) {
              const firstNode = blockLines[lineIndex + 1]?.entries[0]?.node;
              if (firstNode) return {
                node: firstNode, offset: 0, line: blockLines[lineIndex + 1].index
              };
            }
            remaining = Math.max(0, remaining - separatorLength);
          }
        }
        const lastLine = blockLines[blockLines.length - 1]?.entries;
        const lastNode = lastLine?.[lastLine.length - 1]?.node;
        return lastNode
          ? { node: lastNode, offset: lastNode.length, line: endLineIndex }
          : null;
      }
    };
  }

  /** Skip past sentence-ending punctuation and whitespace in the given
   *  direction. Returns an offset inside the adjacent sentence. */
  _skipSentenceBoundary(text, offset, forward) {
    const boundaryPattern = /[\s\n.!?。！？]/;
    let boundaryOffset = offset;
    if (forward) {
      while (boundaryOffset < text.length && boundaryPattern.test(text[boundaryOffset]))
        boundaryOffset++;
    } else {
      while (boundaryOffset > 0 && boundaryPattern.test(text[boundaryOffset - 1]))
        boundaryOffset--;
    }
    return boundaryOffset;
  }

  _pdfSentenceOffsets(text, caretOffset) {
    const clampedCaretOffset = Math.max(0, Math.min(caretOffset, text.length));
    const sentenceBoundaryPattern = /[.!?。！？]|\n\n/g;
    let start = 0, end = text.length, match;
    while ((match = sentenceBoundaryPattern.exec(text)) !== null) {
      if (match.index < clampedCaretOffset) start = match.index + match[0].length;
      else {
        end = match[0] === '\n\n' ? match.index : match.index + match[0].length;
        break;
      }
    }
    while (start < end && /[\s\n]/.test(text[start])) start++;
    while (end > start && /\s/.test(text[end - 1])) end--;
    return { start, end };
  }

  _expandPdfSentence(selection) {
    const context = this._pdfLineContext(selection);
    if (!context) {
      return false;
    }

    const { lines, currentLineIndex } = context;

    const caretPoint = this._resolveCursorPosition(selection.anchorNode, selection.anchorOffset);
    const caretLineBounds = this._lineBounds(lines[currentLineIndex]);
    const columnLeft = caretLineBounds?.left ?? null;

    const model = this._pdfTextRangeModel(lines, 0, lines.length - 1, columnLeft);
    const caretOffset = model.offsetFromPoint(caretPoint.node, caretPoint.offset);
    const offsets = this._pdfSentenceOffsets(model.text, caretOffset);

    const startPoint = model.pointFromOffset(offsets.start);
    const endPoint = model.pointFromOffset(offsets.end);
    if (!startPoint?.node || !endPoint?.node) return false;
    selection.setBaseAndExtent(startPoint.node, startPoint.offset, endPoint.node, endPoint.offset);
    this._markAnchor = { node: startPoint.node, offset: startPoint.offset };
    this._savedFocus = { node: endPoint.node, offset: endPoint.offset };

    this.markActive = true;
    return true;
  }

  _renderedRangeEntries(range) {
    const common = range.commonAncestorContainer || this._root;
    const root = common.nodeType === Node.TEXT_NODE ? common.parentNode : common;
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodeRange = document.createRange();
    const entries = [];
    while (walker.nextNode()) {
      const node = walker.currentNode;
      if (!this._isNavigableTextNode(node)) continue;
      nodeRange.selectNodeContents(node);
      const startsBeforeEnd = range.compareBoundaryPoints(Range.END_TO_START, nodeRange) < 0;
      const endsAfterStart = range.compareBoundaryPoints(Range.START_TO_END, nodeRange) > 0;
      if (!startsBeforeEnd || !endsAfterStart || !this._hasRenderedBox(node)) continue;
      const start = node === range.startContainer ? range.startOffset : 0;
      const end = node === range.endContainer ? range.endOffset : node.length;
      if (start < end) entries.push({ node, start, end });
    }
    return entries;
  }

  _renderedRangeEndpoints(range, entries = this._renderedRangeEntries(range)) {
    if (!entries.length) return null;

    let first = 0;
    let startOffset = entries[0].start;
    while (first < entries.length && !this._preservesWhitespace(entries[first].node)) {
      const text = entries[first].node.textContent || '';
      while (startOffset < entries[first].end && /\s/u.test(text[startOffset])) startOffset++;
      if (startOffset < entries[first].end) break;
      first++;
      startOffset = entries[first]?.start ?? 0;
    }
    let last = entries.length - 1;
    let endOffset = entries[last].end;
    while (last >= first && !this._preservesWhitespace(entries[last].node)) {
      const text = entries[last].node.textContent || '';
      while (endOffset > entries[last].start && /\s/u.test(text[endOffset - 1])) endOffset--;
      if (endOffset > entries[last].start) break;
      last--;
      endOffset = entries[last]?.end ?? 0;
    }
    if (first > last) return null;

    return {
      startNode: entries[first].node,
      startOffset,
      endNode: entries[last].node,
      endOffset
    };
  }

  _expandTo(selection, granularity) {
    if (this.scrollContainer) {
      if (granularity === 'sentenceboundary' && this._expandPdfSentence(selection)) return;
    }
    const range = selection.getRangeAt(0);
    // Anchor on the focus end (the caret after a word-select), not the
    // range start — and resolve element/empty anchors to text, else the
    // sentence modify grabs a different paragraph than the one the caret
    // is visually on.
    const anchor = { node: selection.focusNode, offset: selection.focusOffset };
    const resolved = anchor.node?.nodeType === Node.TEXT_NODE
      ? anchor : this._resolveCursorPosition(range.startContainer, range.startOffset);
    const refNode = resolved.node;
    const length = refNode.nodeType === Node.TEXT_NODE ? refNode.length : refNode.childNodes.length;
    let refOff = Math.min(resolved.offset + 1, length);
    // A reference at/after the sentence terminator is already past that
    // sentence — the forward extend would swallow the next one. Step back
    // into the word that owns the region.
    if (refNode.nodeType === Node.TEXT_NODE) {
      const refText = refNode.textContent;
      while (refOff > 0 && !this._isWordChar(refText[refOff - 1])) refOff--;
    }
    selection.collapse(refNode, refOff);
    selection.modify('extend', 'backward', granularity);
    const nativeStartNode = selection.focusNode, nativeStartOffset = selection.focusOffset;
    selection.collapse(refNode, refOff);
    selection.modify('extend', 'forward', granularity);
    const nativeEndNode = selection.focusNode, nativeEndOffset = selection.focusOffset;
    const nativeRange = document.createRange();
    nativeRange.setStart(nativeStartNode, nativeStartOffset);
    nativeRange.setEnd(nativeEndNode, nativeEndOffset);
    const endpoints = this._renderedRangeEndpoints(nativeRange) || {
      startNode: nativeStartNode,
      startOffset: nativeStartOffset,
      endNode: nativeEndNode,
      endOffset: nativeEndOffset
    };
    const { startNode, startOffset, endNode, endOffset } = endpoints;
    selection.setBaseAndExtent(startNode, startOffset, endNode, endOffset);
    this._markAnchor = { node: startNode, offset: startOffset };
    this._savedFocus = { node: endNode, offset: endOffset };
    this.markActive = true;
  }

  /* ── Navigation ────────────────────────────────────────────── */

  _jumpToEdge(toStart, root) {
    this._cancelCaretTask();
    const selection = this._ensureSelection();
    if (!selection) return;
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
    this._applyRange(selection, range);
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
    this._cancelCaretTask();
    if (this.markActive) {
      this.deactivateMark();
      return false;
    }
    const selection = this._ensureSelection(true);
    this.markActive = true;
    if (selection?.rangeCount) {
      this._markAnchor = { node: selection.focusNode, offset: selection.focusOffset };
      // Keep movement start in sync: a click moved the DOM selection without
      // updating state — a stale _savedFocus would build the region from the
      // pre-click position.
      this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
    } else {
      this._markAnchor = null;
    }
    this._savedCaret = null;
    this._updateCursor();
    return true;
  }

  deactivateMark() {
    this._cancelCaretTask();
    this.markActive = false;
    this._markAnchor = null;
    const selection = window.getSelection();
    const saved = this._savedCaret || this._savedFocus;
    this._savedCaret = null;
    if (saved?.node && this._root.contains(saved.node)) {
      const max = saved.node.nodeType === Node.TEXT_NODE
        ? saved.node.length : saved.node.childNodes.length;
      selection.collapse(saved.node, Math.min(saved.offset, max));
      this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
    } else if (selection?.rangeCount) {
      selection.collapse(selection.focusNode, selection.focusOffset);
      this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
    }
    this._updateCursor();
  }

  expandSelection() {
    this._cancelCaretTask();
    const selection = this._ensureSelection(true);
    if (!selection?.rangeCount) return;

    const scope = this._detectSelectionScope();
    if (scope === 'none') {
      this._savedCaret = { node: selection.focusNode, offset: selection.focusOffset };
      const node = selection.focusNode;
      const focusOffset = selection.focusOffset;
      if (node.nodeType === Node.TEXT_NODE) {
        const text = node.textContent;
        let start = focusOffset, end = focusOffset;
        // Extend start backward over word chars
        while (start > 0 && this._isWordChar(text[start - 1])) start--;
        // Extend end forward over word chars
        while (end < text.length && this._isWordChar(text[end])) end++;
        if (start === end) {
          // Caret sits in a non-word run: prefer the following word, else
          // take the preceding one — without dragging the punctuation run
          // into the region.
          let scan = focusOffset;
          while (scan < text.length && !this._isWordChar(text[scan])) scan++;
          if (scan < text.length) {
            end = scan;
            while (end < text.length && this._isWordChar(text[end])) end++;
            start = end;
            while (start > 0 && this._isWordChar(text[start - 1])) start--;
          } else {
            end = focusOffset;
            while (end > 0 && !this._isWordChar(text[end - 1])) end--;
            start = end;
            while (start > 0 && this._isWordChar(text[start - 1])) start--;
          }
        }
        selection.setBaseAndExtent(node, start, node, end);
        this._markAnchor = { node, offset: start };
        this._savedFocus = { node, offset: end };
      } else {
        selection.modify('move', 'forward', 'word');
        selection.modify('extend', 'backward', 'word');
        // extend-backward leaves focus at the word start and anchor at its
        // end — normalize to the mark=start / point=end invariant.
        selection.setBaseAndExtent(selection.focusNode, selection.focusOffset,
          selection.anchorNode, selection.anchorOffset);
        this._markAnchor = { node: selection.anchorNode, offset: selection.anchorOffset };
        this._savedFocus = { node: selection.focusNode, offset: selection.focusOffset };
      }
      this.markActive = true;
    } else if (scope === 'word') {
      this._expandTo(selection, 'sentenceboundary');
    }
    this._scrollToSelection();
    this._updateCursor();
  }

  moveWithBoundaryCheck(method, granularity) {
    // Boundaries are meaningful only for the dispatched command — don't
    // report a previous movement's outcome.
    this._lastMoved = false;
    this._lastDir = null;
    if (granularity) this[method](granularity);
    else this[method]();
    if (this._lastMoved) return "ok";
    if (this._lastDir === "forward" && (this._hitBoundary || this.isAtBottom())) return "at-end";
    if (this._lastDir === "backward" && (this._hitBoundary || this.isAtTop())) return "at-start";
    return "ok";
  }

  _pageAtViewportEdge(toStart) {
    const viewport = this._viewportRect();
    const x = (viewport.left + viewport.right) / 2;
    const y = toStart
      ? viewport.top + this._viewportEdgeOffset
      : viewport.bottom - this._viewportEdgeOffset;
    return document.elementFromPoint(x, y)?.closest('.page[data-page-number]') || null;
  }

  _jumpToBufferEdge(toStart) {
    this._cancelCaretTask();
    if (!this._isPdfMode()) return this._jumpToEdge(toStart);
    this._scrollTo(toStart ? 0 : this._scrollHeight);
    const page = this._pageAtViewportEdge(toStart);
    if (!page) return;
    const jump = () => this._jumpToEdge(toStart, page);
    if (page.querySelector('.textLayer')?.children.length) {
      jump();
      return;
    }
    const pageNumber = Number(page.dataset.pageNumber);
    const task = this._startCaretTask();
    task.unsubscribe = this._onTextLayerReady(() => {
      this._cancelCaretTask();
      jump();
    }, pageNumber);
  }

  beginningOfBuffer() { this._jumpToBufferEdge(true); }
  endOfBuffer() { this._jumpToBufferEdge(false); }

  getScrollPercent() {
    if (this._isPdfMode()) {
      const page = this._currentPage();
      if (page) {
        const pageRect = page.getBoundingClientRect();
        const selection = window.getSelection();
        let pageOffsetY = -pageRect.top;
        if (selection?.focusNode) {
          const caretRect = this._rangeRectAt(selection.focusNode, selection.focusOffset)
            || this._collapsedRange(selection.focusNode, selection.focusOffset).getBoundingClientRect();
          if (caretRect) pageOffsetY = caretRect.top - pageRect.top;
        }
        const total = pageRect.height;
        return total > 0 ? Math.min(100, Math.max(0, pageOffsetY / total * 100)) : 0;
      }
    }
    const total = this._scrollHeight - this._viewportHeight;
    let scrollOffsetY = this._scrollTop;
    const selection = window.getSelection();
    if (selection?.focusNode) {
      const caretRect = this._rangeRectAt(selection.focusNode, selection.focusOffset)
        || this._collapsedRange(selection.focusNode, selection.focusOffset).getBoundingClientRect();
      if (caretRect) scrollOffsetY = caretRect.top + this._scrollTop;
    }
    return total > 0 ? Math.min(100, scrollOffsetY / total * 100) : 0;
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
    this._cancelCaretTask();
    const selection = window.getSelection();
    const savedPoint = this.markActive ? this._savedFocusPoint() : null;
    const point = savedPoint
      || (selection?.focusNode && this._isContained(selection.focusNode)
        ? { node: selection.focusNode, offset: selection.focusOffset } : null);
    if (!point?.node || !this._isContained(point.node)) return;
    const resolved = this._resolveCursorPosition(point.node, point.offset);
    const rect = this._cursorRectAt(resolved.node, resolved.offset);
    if (!rect?.height) return;
    const x = rect.left + rect.width / 2;
    const y = rect.top + rect.height / 2;
    const el = document.elementFromPoint(x, y);
    if (!el) return;
    el.dispatchEvent(new MouseEvent("click",
      { bubbles: true, cancelable: true, clientX: x, clientY: y }));
  }

  /** Return the absolute href of the anchor at the caret, or "". */
  linkAtCaret() {
    const selection = window.getSelection();
    const saved = this._savedCaret || this._savedFocus || { node: selection.focusNode, offset: selection.focusOffset };
    if (!saved?.node || !this._root.contains(saved.node)) return "";
    const { node, offset } = this._resolveCursorPosition(saved.node, saved.offset);
    const rect = this._cursorRectAt(node, offset);
    if (!rect?.height) return "";
    const el = document.elementFromPoint(rect.left + rect.width / 2, rect.top + rect.height / 2);
    if (!el) return "";
    const anchor = el.closest("a[href]");
    return anchor ? anchor.href : "";
  }

  caretInfo() {
    const selection = window.getSelection();
    const saved = this._savedCaret || this._savedFocus || { node: selection.focusNode, offset: selection.focusOffset };
    if (!saved?.node || !this._root.contains(saved.node))
      return JSON.stringify({ text: "", left: 0, bottom: 0 });
    // Collapsed range at the saved caret
    let caretRect = this._collapsedRange(saved.node, saved.offset).getBoundingClientRect();
    if (!caretRect || (!caretRect.width && !caretRect.height))
      caretRect = this._cursorRectAt(saved.node, saved.offset);
    const left = caretRect?.left ?? 0, bottom = caretRect?.bottom ?? 0;
    let text = "";
    if (!selection.isCollapsed) {
      text = selection.toString();
      this.deactivateMark();
    } else {
      const node = selection.focusNode, offset = selection.focusOffset;
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
    this._cancelCaretTask();
    this._pendingTextRetry?.cancel?.();
    this._pendingTextRetry = null;
    if (this._scrollRaf != null) cancelAnimationFrame(this._scrollRaf);
    this._scrollRaf = null;
    document.removeEventListener("selectionchange", this._onSelectionChange);
    document.removeEventListener("keydown", this._onKeyDown);
    document.removeEventListener("mousedown", this._onMouseDown);
    window.removeEventListener('wheel', this._onWheel, { capture: true });
    if (this.scrollContainer && this._onPdfScroll) {
      this.scrollContainer.removeEventListener("scroll", this._onPdfScroll);
    } else {
      window.removeEventListener("scroll", this._onScroll);
    }
    window.removeEventListener("resize", this._onResize);
    window.removeEventListener("pageshow", this._onPageShow);
    window.removeEventListener("hashchange", this._onHashChange);
    this._cursorEl?.remove();
    this._cursorEl = null;
    this._visualOrderCache = { root: null, layoutGeneration: -1, ordered: null, lines: null };
  }
}

window.CaretEmacs = CaretEmacs;
function initCaretEmacs() {
  const viewerContainer = document.getElementById('viewerContainer');
  const viewer = document.getElementById('viewer');
  window.__caretEmacs?.destroy();
  window.__caretEmacs = viewerContainer && viewer
    ? new CaretEmacs(viewer, { scrollContainer: viewerContainer })
    : new CaretEmacs(document);
}
if (!window.__caretEmacsTest) initCaretEmacs();
