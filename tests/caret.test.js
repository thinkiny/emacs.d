const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const sourcePath = path.join(__dirname, '..', 'assets', 'js', 'caret.js');
const source = fs.readFileSync(sourcePath, 'utf8');

const context = {
  console,
  location: { href: 'https://example.test/page', hash: '' },
  history: {},
  localStorage: {},
  performance: { now: () => 0 },
  Node: {
    ELEMENT_NODE: 1,
    TEXT_NODE: 3,
    DOCUMENT_POSITION_FOLLOWING: 4,
  },
  NodeFilter: { SHOW_TEXT: 4 },
  Range: {
    START_TO_START: 0,
    START_TO_END: 1,
    END_TO_END: 2,
    END_TO_START: 3,
  },
  document: {},
  window: { __caretEmacsTest: true },
  getComputedStyle: (element) => element?._style || {
    fontSize: '16px',
    position: 'static',
    whiteSpace: 'normal',
  },
  requestAnimationFrame: () => 1,
  cancelAnimationFrame: () => {},
  MouseEvent: class MouseEvent {
    constructor(type, options) { this.type = type; Object.assign(this, options); }
  },
  setTimeout,
  clearTimeout,
};
vm.createContext(context);
vm.runInContext(source, context);
const CaretEmacs = context.window.CaretEmacs;

function append(parent, child) {
  parent.childNodes.push(child);
  child.parentNode = parent;
  child.parentElement = parent.nodeType === 1 ? parent : parent.parentElement;
  if (!parent.firstChild) parent.firstChild = child;
  parent.lastChild = child;
  return child;
}

function element(tagName = 'DIV', id = '') {
  return {
    nodeType: 1,
    tagName,
    id,
    childNodes: [],
    firstChild: null,
    lastChild: null,
    parentNode: null,
    parentElement: null,
    textContent: '',
    _style: { fontSize: '16px', position: 'static', whiteSpace: 'normal' },
    contains(node) {
      for (let current = node; current; current = current.parentNode) {
        if (current === this) return true;
      }
      return false;
    },
  };
}

function text(value) {
  return {
    nodeType: 3,
    textContent: value,
    get length() { return this.textContent.length; },
    parentNode: null,
    parentElement: null,
  };
}

function textNodes(root) {
  const nodes = [];
  const visit = (node) => {
    if (node.nodeType === 3) nodes.push(node);
    else for (const child of node.childNodes || []) visit(child);
  };
  visit(root);
  return nodes;
}

function makeRange() {
  return {
    startContainer: null,
    startOffset: 0,
    endContainer: null,
    endOffset: 0,
    setStart(node, offset) {
      this.startContainer = node;
      this.startOffset = offset;
      if (!this.endContainer) {
        this.endContainer = node;
        this.endOffset = offset;
      }
    },
    setEnd(node, offset) {
      this.endContainer = node;
      this.endOffset = offset;
    },
    selectNodeContents(node) {
      this.startContainer = node;
      this.startOffset = 0;
      this.endContainer = node;
      this.endOffset = node.nodeType === 3 ? node.length : node.childNodes.length;
    },
    collapse(toStart) {
      if (toStart) {
        this.endContainer = this.startContainer;
        this.endOffset = this.startOffset;
      } else {
        this.startContainer = this.endContainer;
        this.startOffset = this.endOffset;
      }
    },
    cloneRange() {
      const copy = makeRange();
      Object.assign(copy, {
        startContainer: this.startContainer,
        startOffset: this.startOffset,
        endContainer: this.endContainer,
        endOffset: this.endOffset,
      });
      return copy;
    },
    getClientRects: () => [],
    getBoundingClientRect: () => ({ top: 0, bottom: 10, left: 0, width: 1, height: 10 }),
  };
}

function makeDocument(root = null) {
  const listeners = new Map();
  const document = {
    listeners,
    body: null,
    head: { appendChild() {} },
    documentElement: {
      scrollHeight: 1000,
      appendChild(node) { node.isConnected = true; },
    },
    addEventListener(type, callback) { listeners.set(type, callback); },
    removeEventListener(type) { listeners.delete(type); },
    createElement() { return { style: {}, remove() {}, isConnected: true }; },
    createRange: makeRange,
    createTreeWalker(walkerRoot) {
      const nodes = textNodes(walkerRoot);
      let index = -1;
      let currentNode = walkerRoot;
      return {
        get currentNode() { return currentNode; },
        set currentNode(node) {
          currentNode = node;
          index = nodes.indexOf(node);
        },
        nextNode() {
          index += 1;
          if (index >= nodes.length) return null;
          currentNode = nodes[index];
          return currentNode;
        },
        previousNode() {
          index -= 1;
          if (index < 0) return null;
          currentNode = nodes[index];
          return currentNode;
        },
      };
    },
    getElementById(id) {
      const visit = (node) => {
        if (!node) return null;
        if (node.id === id) return node;
        for (const child of node.childNodes || []) {
          const found = visit(child);
          if (found) return found;
        }
        return null;
      };
      return visit(root);
    },
    getElementsByName() { return []; },
    querySelector() { return null; },
    elementFromPoint() { return null; },
  };
  return document;
}

function makeSelection(node = null, offset = 0) {
  return {
    rangeCount: node ? 1 : 0,
    focusNode: node,
    focusOffset: offset,
    anchorNode: node,
    anchorOffset: offset,
    isCollapsed: true,
    collapse(nextNode, nextOffset) {
      this.rangeCount = 1;
      this.focusNode = nextNode;
      this.focusOffset = nextOffset;
      this.anchorNode = nextNode;
      this.anchorOffset = nextOffset;
      this.isCollapsed = true;
    },
    removeAllRanges() { this.rangeCount = 0; },
    addRange(range) {
      this.rangeCount = 1;
      this.focusNode = range.startContainer;
      this.focusOffset = range.startOffset;
      this.anchorNode = range.startContainer;
      this.anchorOffset = range.startOffset;
      this.isCollapsed = true;
    },
    setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset) {
      this.rangeCount = 1;
      this.anchorNode = anchorNode;
      this.anchorOffset = anchorOffset;
      this.focusNode = focusNode;
      this.focusOffset = focusOffset;
      this.isCollapsed = anchorNode === focusNode && anchorOffset === focusOffset;
    },
    getRangeAt() {
      const range = makeRange();
      range.setStart(this.focusNode, this.focusOffset);
      return range;
    },
    toString() { return ''; },
  };
}

function installClock() {
  let now = 0;
  let nextId = 1;
  const timers = new Map();
  const frames = new Map();
  context.performance = { now: () => now };
  context.setTimeout = (callback, delay = 0) => {
    const id = nextId++;
    timers.set(id, { callback, at: now + delay });
    return id;
  };
  context.clearTimeout = (id) => timers.delete(id);
  context.requestAnimationFrame = (callback) => {
    const id = nextId++;
    frames.set(id, callback);
    return id;
  };
  context.cancelAnimationFrame = (id) => frames.delete(id);
  return {
    get now() { return now; },
    pendingTimers: () => timers.size,
    pendingFrames: () => frames.size,
    runNextTimer() {
      const entry = [...timers.entries()].sort((a, b) => a[1].at - b[1].at)[0];
      if (!entry) return false;
      const [id, task] = entry;
      timers.delete(id);
      now = task.at;
      task.callback();
      return true;
    },
    runTimers(limit = 200) {
      let count = 0;
      while (timers.size && count < limit) {
        this.runNextTimer();
        count += 1;
      }
      if (timers.size) throw new Error(`timer queue did not settle after ${limit} callbacks`);
    },
    runFrames() {
      const pending = [...frames.entries()];
      frames.clear();
      for (const [, callback] of pending) callback(now);
    },
  };
}

function resetEnvironment(root = element('BODY')) {
  const document = makeDocument(root);
  const selection = makeSelection();
  const windowListeners = new Map();
  context.document = document;
  context.window = {
    __caretEmacsTest: true,
    innerWidth: 800,
    innerHeight: 600,
    scrollX: 0,
    scrollY: 0,
    getSelection: () => selection,
    addEventListener(type, callback) { windowListeners.set(type, callback); },
    removeEventListener(type) { windowListeners.delete(type); },
    scrollBy() {},
    scrollTo() {},
  };
  context.location = { href: 'https://example.test/page', hash: '' };
  context.history = {};
  const storage = new Map();
  context.localStorage = {
    getItem: (key) => storage.has(key) ? storage.get(key) : null,
    setItem: (key, value) => storage.set(key, String(value)),
  };
  return { document, root, selection, storage, windowListeners };
}

function bareCaret(root) {
  const caret = Object.create(CaretEmacs.prototype);
  Object.assign(caret, {
    el: root,
    scrollContainer: null,
    markActive: false,
    _markAnchor: null,
    _savedCaret: null,
    _savedFocus: null,
    _caretTask: null,
    _scrollRaf: null,
    _suppressScrollRelocate: false,
    _lastScrollTop: 0,
    _caretViewportTop: null,
    _historyPositions: new Map(),
    _historyLocation: context.location.href,
    _fontSizeCache: new WeakMap(),
    _textBoundsCache: new WeakMap(),
    _visualOrderCache: { root: null, layoutGeneration: -1, ordered: null, lines: null },
    _layoutGeneration: 0,
    _scrollPx: 200,
    _scrollDownFraction: 1 / 3,
    _scrollUpFraction: 2 / 3,
    _viewportEdgeOffset: 20,
  });
  return caret;
}

function assertPoint(actual, node, offset) {
  assert.equal(actual?.node, node);
  assert.equal(actual?.offset, offset);
}

function deferredInstance(root) {
  context.document.body = null;
  return new CaretEmacs(root);
}

test('stored offsets accept only safe non-negative decimal integers', () => {
  const { root, storage } = resetEnvironment();
  const caret = bareCaret(root);
  caret._caretStorageKey = () => 'caret';
  for (const value of ['-1', '12px', '1.5', '', '9007199254740992']) {
    storage.set('caret', value);
    assert.equal(caret._storedCaretPosition(), null, value);
  }
  storage.set('caret', ' 12 ');
  assert.equal(caret._storedCaretPosition().offset, 12);
});

test('global offsets reject a hidden target node', () => {
  const root = element('BODY');
  const visible = append(root, text('hello'));
  const hidden = append(root, text('secret'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = (node) => node !== hidden;
  assert.equal(caret._caretGlobalOffset(hidden, 2), null);
});

test('persisted restore follows hydration node replacement before completing', () => {
  const root = element('BODY');
  const first = append(root, text('first target'));
  const final = append(root, text('final target'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  const caret = bareCaret(root);
  storage.set('caret', '7');
  caret._caretStorageKey = () => 'caret';
  caret._resolveGlobalOffset = () => {
    if (clock.now < 100) return null;
    return clock.now < 200
      ? { node: first, offset: 2 }
      : { node: final, offset: 3 };
  };
  caret._collapsedRange = (node, offset) => ({ startContainer: node, startOffset: offset });
  caret._setSelectionRange = (sel, range) => sel.collapse(range.startContainer, range.startOffset);
  caret._isContained = (node) => root.contains(node);
  caret._hasRenderedBox = () => true;
  caret._selectionFocusRect = () => ({ top: 10, bottom: 20, height: 10 });
  caret._isRectInViewport = () => true;
  caret._scrollToSelection = () => {};
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });

  assert.equal(caret._restoreCaret(), true);
  clock.runTimers();
  assert.equal(selection.focusNode, final);
  assert.equal(selection.focusOffset, 3);
});

test('restore timeout protects storage until explicit cancellation', () => {
  const root = element('BODY');
  const fallback = append(root, text('fallback'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(fallback, 1);
  const caret = bareCaret(root);
  caret._cursorEl = { isConnected: true, style: { display: 'block' } };
  caret._lastRenderedPos = { node: fallback, offset: 1 };
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  storage.set('caret', '20');
  caret._caretStorageKey = () => 'caret';
  caret._resolveGlobalOffset = () => null;
  caret._ensureSelection = () => selection;
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });

  assert.equal(caret._restoreCaret(), true);
  clock.runTimers();
  assert.equal(storage.get('caret'), '20');
  caret._cancelCaretTask();
  caret._updateCursor();
  assert.deepEqual(JSON.parse(storage.get('caret')), { offset: 1, nextBoundary: false });
});

test('scroll dispatch updates geometry without relocating during restore', () => {
  const root = element('BODY');
  const point = append(root, text('point'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(point, 1);
  const caret = bareCaret(root);
  let invalidations = 0;
  let probes = 0;
  storage.set('caret', '20');
  caret._caretStorageKey = () => 'caret';
  caret._resolveGlobalOffset = () => null;
  assert.equal(caret._restoreCaret(), true);
  caret._lastScrollTop = 0;
  caret._invalidateLayoutCaches = () => { invalidations += 1; };
  caret._probeTextAt = () => { probes += 1; return null; };
  Object.defineProperty(caret, '_scrollTop', { get: () => 50 });

  caret._onUserScroll();
  clock.runFrames();
  assert.equal(invalidations, 1);
  assert.equal(caret._lastScrollTop, 50);
  assert.equal(probes, 0);
});

test('wheel input cancels restore and scroll adopts the landed point', () => {
  const root = element('BODY');
  const restored = append(root, text('restored'));
  const landed = append(root, text('landed'));
  const { document, selection, windowListeners } = resetEnvironment(root);
  const clock = installClock();
  document.body = root;
  selection.collapse(restored, 1);
  const caret = new CaretEmacs(root);
  caret._beginStoredRestore(3, 1000, { globalOffset: 3 });
  caret._lastScrollTop = 0;
  caret._rangeRectAt = (node) => node === restored
    ? { top: -100, bottom: -90, left: 10, width: 1, height: 10 }
    : { top: 20, bottom: 30, left: 10, width: 1, height: 10 };
  caret._viewportRect = () => ({ top: 0, bottom: 100, left: 0, right: 800 });
  caret._probeTextAt = () => ({ startContainer: landed, startOffset: 2 });
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 50 });

  windowListeners.get('wheel')();
  assert.equal(caret._caretTask, null);
  assert.equal(clock.pendingTimers(), 0);
  caret._onUserScroll();
  clock.runFrames();

  assertPoint(caret._savedFocus, landed, 2);
  assert.equal(selection.focusNode, landed);
  assert.equal(selection.focusOffset, 2);
});

test('destroy cancels the owned task timeout and pending scroll frame', () => {
  const { root, document } = resetEnvironment();
  const clock = installClock();
  const caret = bareCaret(root);
  let called = false;
  const task = caret._startCaretTask();
  caret._scheduleCaretTask(task, () => { called = true; }, 10);
  caret._scrollRaf = context.requestAnimationFrame(() => { called = true; });
  Object.assign(caret, {
    _onSelectionChange() {},
    _onKeyDown() {},
    _onMouseDown() {},
    _onScroll() {},
    _onResize() {},
    _onPageShow() {},
    _onHashChange() {},
    _cursorEl: null,
  });
  document.body = root;

  caret.destroy();
  clock.runTimers();
  clock.runFrames();
  assert.equal(called, false);
});

test('malformed hashes are safe and delayed targets are re-queried', () => {
  const root = element('BODY');
  const { document, selection } = resetEnvironment(root);
  const clock = installClock();
  const caret = bareCaret(root);
  context.location.hash = '#100%';
  assert.equal(caret._decodedHash(), '100%');

  context.location.hash = '#late';
  let targetElement = null;
  document.getElementById = () => targetElement;
  caret._isContained = (node) => root.contains(node);
  caret._hasRenderedBox = () => true;
  caret._collapsedRange = (node, offset) => ({ startContainer: node, startOffset: offset });
  caret._setSelectionRange = (sel, range) => sel.collapse(range.startContainer, range.startOffset);
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });

  assert.equal(caret._restoreHashTarget(), true);
  assert.equal(selection.focusNode, null);
  targetElement = append(root, element('H2', 'late'));
  const targetText = append(targetElement, text('Late heading'));
  clock.runTimers();
  assert.equal(selection.focusNode, targetText);
});

test('pageshow replaces stale work and restarts an unresolved hash target', () => {
  const root = element('BODY');
  const oldPoint = append(root, text('old point'));
  resetEnvironment(root);
  const clock = installClock();
  const caret = deferredInstance(root);
  let staleCallbackRan = false;
  let hashRestarts = 0;
  const staleTask = caret._startCaretTask({ hash: 'target' });
  caret._scheduleCaretTask(staleTask, () => { staleCallbackRan = true; }, 50);
  caret._savedFocus = { node: oldPoint, offset: 2 };
  caret._hasRenderedBox = () => true;
  caret._restoreHashTarget = () => { hashRestarts += 1; return true; };
  context.location.hash = '#target';

  caret._onPageShow({ persisted: true });
  clock.runTimers();
  assert.equal(staleCallbackRan, false);
  assert.equal(hashRestarts, 1);
});

test('an empty decoded hash falls through to ordinary restoration', () => {
  const { root } = resetEnvironment();
  installClock();
  const caret = bareCaret(root);
  let ensured = 0;
  let revealed = 0;
  caret._cancelCaretTask = () => {};
  caret._restoreHashTarget = () => false;
  caret._restoreCaret = () => false;
  caret._ensureSelection = () => { ensured += 1; };
  caret._revealCaret = () => { revealed += 1; };
  context.location.hash = '#';

  caret.restore();
  assert.equal(ensured, 1);
  assert.equal(revealed, 1);
});

test('resize paths prefer a valid replacement and reject a shifted path', () => {
  const root = element('BODY');
  const anchor = append(root, element('ARTICLE', 'article'));
  const paragraph = append(anchor, element('P'));
  const original = append(paragraph, text('original'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;

  const saved = caret._captureTextBookmark({ node: original, offset: 5 });
  const replacement = text('replacement text');
  paragraph.childNodes = [];
  append(paragraph, replacement);
  original.parentNode = null;
  original.parentElement = null;
  const resolvedReplacement = caret._resolveTextBookmark(saved);
  assertPoint(resolvedReplacement, replacement, 5);

  const fallback = { node: replacement, offset: 1 };
  caret._resolveGlobalOffset = () => fallback;
  paragraph.childNodes = [];
  append(paragraph, text('inserted'));
  append(paragraph, replacement);
  const resolvedFallback = caret._resolveTextBookmark(saved);
  assertPoint(resolvedFallback, fallback.node, fallback.offset);
  assert.equal(resolvedFallback.viaOffset, true);
});

test('resize during persisted restore retains global-offset identity', () => {
  const root = element('BODY');
  const paragraph = append(root, element('P', 'point'));
  const point = append(paragraph, text('responsive point'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(point, 4);
  const caret = deferredInstance(root);
  caret._savedFocus = { node: point, offset: 4 };
  caret._caretViewportTop = 30;
  caret._caretStorageKey = () => 'caret';
  storage.set('caret', '4');
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._selectionFocusRect = () => ({ top: 30, bottom: 40, height: 10 });
  caret._isRectInViewport = () => true;
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });

  assert.equal(caret._restoreCaret(), true);
  caret._onResize();

  const inserted = text('content inserted before the stable anchor');
  const replacement = text('responsive replacement');
  root.childNodes = [];
  append(root, inserted);
  append(root, paragraph);
  paragraph.childNodes = [];
  append(paragraph, replacement);
  point.parentNode = null;
  point.parentElement = null;
  selection.collapse(inserted, 0);
  clock.runTimers();
  assertPoint(caret._savedFocus, inserted, 4);
});

test('resize dispatch restores point and mark through stable DOM paths', () => {
  const root = element('BODY');
  const top = append(root, text('top'));
  const markElement = append(root, element('P', 'mark'));
  const pointElement = append(root, element('P', 'point'));
  const oldMark = append(markElement, text('old mark'));
  const oldPoint = append(pointElement, text('old point'));
  const { selection } = resetEnvironment(root);
  const clock = installClock();
  selection.setBaseAndExtent(oldMark, 2, oldPoint, 4);
  const caret = deferredInstance(root);
  caret.markActive = true;
  caret._markAnchor = { node: oldMark, offset: 2 };
  caret._savedFocus = { node: oldPoint, offset: 4 };
  caret._savedCaret = { node: oldPoint, offset: 1 };
  caret._caretViewportTop = 20;
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._selectionFocusRect = () => ({ top: 20, bottom: 30, height: 10 });
  caret._isRectInViewport = () => true;
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });

  caret._onResize();
  const newMark = text('replacement mark');
  const newPoint = text('replacement point');
  markElement.childNodes = [];
  pointElement.childNodes = [];
  append(markElement, newMark);
  append(pointElement, newPoint);
  oldMark.parentNode = null;
  oldMark.parentElement = null;
  oldPoint.parentNode = null;
  oldPoint.parentElement = null;
  selection.collapse(top, 0);

  clock.runTimers();
  assert.equal(caret.markActive, true);
  assertPoint(caret._markAnchor, newMark, 2);
  assertPoint(caret._savedFocus, newPoint, 4);
  assert.equal(selection.anchorNode, newMark);
  assert.equal(selection.focusNode, newPoint);
});

test('mousedown synchronizes point after deactivating mark', () => {
  const root = element('BODY');
  const oldPoint = append(root, text('old'));
  const clickedPoint = append(root, text('clicked'));
  const { selection } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(oldPoint, 1);
  const caret = deferredInstance(root);
  caret.markActive = true;
  caret._markAnchor = { node: oldPoint, offset: 1 };
  caret._savedFocus = { node: oldPoint, offset: 1 };
  caret._updateCursor = () => {};

  caret._onMouseDown();
  selection.collapse(clickedPoint, 3);
  clock.runTimers();
  assertPoint(caret._savedFocus, clickedPoint, 3);
});

test('ignored fallback scrolling does not request a line retry', () => {
  const { root } = resetEnvironment();
  const caret = bareCaret(root);
  caret._canScroll = () => true;
  caret._scrollBy = () => 0;
  const result = caret._scrollForLineRetry(true, 10);
  assert.equal(result.kind, 'boundary');
});

test('line retry keeps goal X local and recomputes against a fresh model', () => {
  const root = element('BODY');
  const start = append(root, text('start'));
  const target = append(root, text('target'));
  const { selection } = resetEnvironment(root);
  selection.collapse(start, 1);
  const caret = bareCaret(root);
  caret._cancelCaretTask = () => {};
  caret._ensureSelection = () => selection;
  caret._isAtVisibleBoundary = () => false;
  caret._resolveCursorPosition = (node, offset) => ({ node, offset });
  caret._rangeRectAt = (node) => node === target
    ? { top: 20, bottom: 30, left: 0, width: 1, height: 10 }
    : { top: 0, bottom: 10, left: 0, width: 1, height: 10 };
  caret._collapsedRange = (node, offset) => ({
    startContainer: node,
    startOffset: offset,
    getBoundingClientRect: () => caret._rangeRectAt(node),
  });
  caret._viewportRect = () => ({ top: -10, bottom: 100 });
  caret._isSameLine = () => false;
  caret._finishMove = (_sel, moved) => moved;
  const calls = [];
  caret._moveLine = (...args) => {
    calls.push(args);
    if (calls.length === 1) return { kind: 'retry', goalX: 42 };
    return {
      kind: 'moved',
      range: { startContainer: target, startOffset: 2 },
      gapJump: false,
    };
  };

  assert.equal(caret._moveCaret('forward', 'line'), true);
  assert.equal(calls.length, 2);
  assert.equal(calls[1][1], 42);
});

test('rendered sentence endpoints stay inside the native range', () => {
  const root = element('BODY');
  const before = append(root, text('before'));
  const first = append(root, text('  Alpha '));
  const hidden = append(root, text('hidden.'));
  const terminal = append(root, text('ends. trailing'));
  const following = append(root, text(' Next'));
  const after = append(root, text('after'));
  const { document } = resetEnvironment(root);
  const caret = bareCaret(root);
  const nodes = [before, first, hidden, terminal, following, after];
  const index = new Map(nodes.map((node, nodeIndex) => [node, nodeIndex]));
  document.createRange = () => {
    const range = makeRange();
    range.selectNodeContents = function selectNodeContents(node) {
      this.startContainer = node;
      this.startOffset = 0;
      this.endContainer = node;
      this.endOffset = node.length;
    };
    return range;
  };
  const pointValue = (node, offset) => {
    if (node === root) return offset * 2;
    const nodeIndex = index.get(node);
    return nodeIndex * 2 + (offset > 0 ? 1 : 0);
  };
  const nativeRange = {
    commonAncestorContainer: root,
    startContainer: root,
    startOffset: 1,
    endContainer: root,
    endOffset: 5,
    compareBoundaryPoints(how, nodeRange) {
      if (how === context.Range.START_TO_END) {
        return pointValue(this.endContainer, this.endOffset)
          - pointValue(nodeRange.startContainer, nodeRange.startOffset);
      }
      if (how === context.Range.END_TO_START) {
        return pointValue(this.startContainer, this.startOffset)
          - pointValue(nodeRange.endContainer, nodeRange.endOffset);
      }
      throw new Error(`unexpected comparison ${how}`);
    },
  };
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = (node) => node !== hidden;
  caret._preservesWhitespace = () => false;

  const result = caret._renderedRangeEndpoints(nativeRange);
  assertPoint({ node: result?.startNode, offset: result?.startOffset }, first, 2);
  assertPoint({ node: result?.endNode, offset: result?.endOffset }, following, 5);
});

test('rendered endpoints retain boundary whitespace in preserved text', () => {
  const root = element('BODY');
  const node = append(root, text('  value  '));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._preservesWhitespace = () => true;
  const nativeRange = {
    commonAncestorContainer: node,
    startContainer: node,
    startOffset: 0,
    endContainer: node,
    endOffset: node.length,
    compareBoundaryPoints(how, nodeRange) {
      if (how === context.Range.START_TO_END)
        return this.endOffset - nodeRange.startOffset;
      if (how === context.Range.END_TO_START)
        return this.startOffset - nodeRange.endOffset;
      throw new Error(`unexpected comparison ${how}`);
    },
  };

  const result = caret._renderedRangeEndpoints(nativeRange);
  assertPoint({ node: result?.startNode, offset: result?.startOffset }, node, 0);
  assertPoint({ node: result?.endNode, offset: result?.endOffset }, node, node.length);
});

test('sentence expansion keeps native endpoints when rendered normalization has no result', () => {
  const root = element('BODY');
  const node = append(root, text('sentence'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret.scrollContainer = null;
  caret._renderedRangeEndpoints = () => null;
  caret._resolveCursorPosition = (current, offset) => ({ node: current, offset });
  caret._textVisibleEdgeOffset = () => 4;
  const selection = makeSelection(node, 2);
  selection.getRangeAt = () => ({ startContainer: node, startOffset: 2 });
  let direction = '';
  selection.modify = (_alter, nextDirection) => {
    direction = nextDirection;
    selection.focusNode = node;
    selection.focusOffset = direction === 'backward' ? 0 : 8;
  };

  caret._expandTo(selection, 'sentenceboundary');
  assertPoint(caret._markAnchor, node, 0);
  assertPoint(caret._savedFocus, node, 8);
});

test('same-position redraw still seeds persistence and history after reveal', () => {
  const root = element('BODY');
  const node = append(root, text('point'));
  const { selection, storage } = resetEnvironment(root);
  selection.collapse(node, 2);
  const caret = bareCaret(root);
  caret._cursorEl = { isConnected: true, style: { display: 'block' } };
  caret._lastRenderedPos = { node, offset: 2 };
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._caretStorageKey = () => 'caret';
  caret._resolveGlobalOffset = () => null;
  storage.set('caret', '20');

  assert.equal(caret._restoreCaret(), true);
  caret._updateCursor();
  assert.equal(storage.get('caret'), '20');
  assert.equal(caret._historyPositions.size, 0);
  caret._cancelCaretTask();
  caret._updateCursor();
  assert.deepEqual(JSON.parse(storage.get('caret')), { offset: 2, nextBoundary: false });
  assertPoint(caret._historyPositions.get(context.location.href), node, 2);
});

test('boundary movement preserves an active region', () => {
  const root = element('BODY');
  const node = append(root, text('region'));
  const { selection } = resetEnvironment(root);
  selection.setBaseAndExtent(node, 0, node, 6);
  const caret = bareCaret(root);
  caret.markActive = true;
  caret._markAnchor = { node, offset: 0 };
  caret._savedFocus = { node, offset: 6 };
  caret._cancelCaretTask = () => {};
  caret._ensureSelection = () => selection;
  caret._isAtVisibleBoundary = () => true;

  assert.equal(caret._moveCaret('forward', 'character'), false);
  assert.equal(selection.isCollapsed, false);
  assert.equal(selection.anchorOffset, 0);
  assert.equal(selection.focusOffset, 6);
});

test('one line command performs at most one fallback scroll step', () => {
  const root = element('BODY');
  const node = append(root, text('point'));
  const { selection } = resetEnvironment(root);
  selection.collapse(node, 1);
  const caret = bareCaret(root);
  let scrolls = 0;
  caret._cancelCaretTask = () => {};
  caret._ensureSelection = () => selection;
  caret._isAtVisibleBoundary = () => false;
  caret._rangeRectAt = () => ({ top: 0, bottom: 10, left: 20, width: 1, height: 10 });
  caret._collapsedRange = (current, offset) => ({
    startContainer: current, startOffset: offset,
    getBoundingClientRect: () => caret._rangeRectAt(current, offset),
  });
  caret._lineMoveRect = () => caret._rangeRectAt();
  caret._moveWithinPreservedTextNode = () => null;
  caret._currentPage = () => null;
  const line = [{
    node, startOffset: 0, endOffset: node.length,
    rect: { top: 0, bottom: 10, left: 20, width: 20, height: 10 },
  }];
  caret._visuallyOrderedTextNodes = () => ({ ordered: line, lines: [line] });
  caret._findCaretLine = () => 0;
  caret._lineTargetIndex = () => -1;
  caret._canScroll = () => true;
  caret._scrollBy = () => { scrolls += 1; return 200; };

  assert.equal(caret._moveCaret('forward', 'line'), true);
  assert.equal(scrolls, 1);
});

test('rendered normalization retains abbreviations inside the native sentence', () => {
  const root = element('BODY');
  const node = append(root, text('Dr. Smith arrived.'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._preservesWhitespace = () => false;
  const nativeRange = {
    commonAncestorContainer: root,
    startContainer: node,
    startOffset: 0,
    endContainer: node,
    endOffset: node.length,
    compareBoundaryPoints(how, nodeRange) {
      if (how === context.Range.START_TO_END) return this.endOffset - nodeRange.startOffset;
      if (how === context.Range.END_TO_START) return this.startOffset - nodeRange.endOffset;
      throw new Error(`unexpected comparison ${how}`);
    },
  };

  const result = caret._renderedRangeEndpoints(nativeRange);
  assertPoint({ node: result.endNode, offset: result.endOffset }, node, node.length);
});

test('resize resumes an in-flight hash restoration', () => {
  const root = element('BODY');
  const oldPoint = append(root, text('old point'));
  const target = append(root, element('H2', 'late'));
  const { document, selection } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(oldPoint, 2);
  const caret = deferredInstance(root);
  caret._savedFocus = { node: oldPoint, offset: 2 };
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._walkToVisible = () => null;
  caret._selectionFocusRect = () => ({ top: 10, bottom: 20, height: 10 });
  caret._isRectInViewport = () => true;
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });
  context.location.hash = '#late';
  document.getElementById = () => target;

  assert.equal(caret._restoreHashTarget(), true);
  caret._onResize();
  const hydrated = append(target, text('late heading'));
  clock.runTimers();
  assert.equal(selection.focusNode, hydrated);
});

test('empty fragment anchors resolve to following rendered text', () => {
  const root = element('BODY');
  const anchor = append(root, element('A'));
  const heading = append(root, element('H2'));
  const headingText = append(heading, text('Chapter'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._textVisibleEdgeOffset = () => 0;
  caret._walkToVisible = (node, fwd) => node === anchor && fwd ? headingText : null;

  assert.equal(caret._firstRenderedTextWithin(anchor), null);
  assertPoint(caret._firstRenderedTextAfter(anchor), headingText, 0);
});

test('page scroll uses actual movement and releases suppression at a boundary', () => {
  const root = element('BODY');
  const node = append(root, text('point'));
  const { selection } = resetEnvironment(root);
  selection.collapse(node, 1);
  const caret = bareCaret(root);
  caret._cancelCaretTask = () => {};
  caret._ensureSelection = () => selection;
  caret._selectionFocusRect = () => ({ top: 100, bottom: 110, left: 20, width: 1, height: 10 });
  caret._viewportRect = () => ({ top: 0, bottom: 300, left: 0, right: 800 });
  Object.defineProperty(caret, '_viewportHeight', { get: () => 300 });
  caret._scrollBy = () => 0;
  caret._updateCursor = () => {};

  caret._scrollPage('down');
  assert.equal(caret._suppressScrollRelocate, false);
  assert.equal(selection.focusNode, node);
  assert.equal(selection.focusOffset, 1);
});

test('cloned rectangles retain bottom for viewport-band filtering', () => {
  const root = element('BODY');
  resetEnvironment(root);
  const caret = bareCaret(root);
  const originalCreateRange = context.document.createRange;
  context.window.innerHeight = 100;
  const far = append(root, text('far'));
  let refined = 0;
  context.document.createRange = () => ({
    selectNodeContents() {},
    getBoundingClientRect: () => ({ top: -1000, bottom: -980, left: 0, width: 10, height: 20 }),
  });
  caret._isNavigableTextNode = () => true;
  caret._lineGroupingRoot = () => root;
  caret._splitNodeFragments = () => { refined += 1; return []; };
  caret._groupIntoLines = (nodes) => nodes.map((entry) => [entry]);
  caret._viewportRect = () => ({ top: 0, bottom: 100, left: 0, right: 800 });
  Object.defineProperty(caret, '_viewportHeight', { get: () => 100 });
  caret._buildVisuallyOrderedTextNodes(root);
  context.document.createRange = originalCreateRange;
  assert.equal(far.length > 0, true);
  assert.equal(refined, 0);
});

test('resize captures final live drag focus instead of stale saved focus', () => {
  const root = element('BODY');
  const initial = append(root, text('initial'));
  const final = append(root, text('final'));
  const { selection } = resetEnvironment(root);
  selection.setBaseAndExtent(initial, 1, final, 3);
  const caret = bareCaret(root);
  caret._savedFocus = { node: initial, offset: 1 };
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;

  const snapshot = caret._captureResizeRestore(null);
  assert.equal(snapshot.point.node, final);
  assert.equal(snapshot.point.offset, 3);
});

test('new caret input cancels pending PDF buffer-edge readiness', () => {
  const root = element('DIV');
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret.scrollContainer = {
    scrollTop: 0, scrollHeight: 1000, clientHeight: 300,
    getBoundingClientRect: () => ({ top: 0, bottom: 300, left: 0, right: 800 }),
  };
  const destination = {
    dataset: { pageNumber: '2' },
    querySelector: () => ({ children: [] }),
  };
  caret._scrollTo = () => {};
  caret._pageAtViewportEdge = () => destination;
  let listener = null;
  let cancelled = 0;
  caret._onTextLayerReady = (callback) => {
    listener = callback;
    return () => { cancelled += 1; listener = null; };
  };
  let jumped = 0;
  caret._jumpToEdge = () => { jumped += 1; };

  caret._jumpToBufferEdge(false);
  caret._cancelCaretTask();
  listener?.();
  assert.equal(cancelled, 1);
  assert.equal(jumped, 0);
});

test('text-layer readiness waits for the requested PDF page', () => {
  const { root } = resetEnvironment();
  const caret = bareCaret(root);
  caret._cursorEl = {};
  const handlers = new Map();
  const bus = {
    on(name, callback) { handlers.set(name, callback); },
    off(name, callback) { if (handlers.get(name) === callback) handlers.delete(name); },
  };
  context.window.PDFViewerApplication = { page: 3, eventBus: bus };
  let called = 0;
  caret._onTextLayerReady(() => { called += 1; }, 3);
  handlers.get('textlayerrendered')({ pageNumber: 2 });
  assert.equal(called, 0);
  handlers.get('textlayerrendered')({ pageNumber: 3 });
  assert.equal(called, 1);
});

test('clickAtCaret clicks point rather than the range start', () => {
  const root = element('BODY');
  const mark = append(root, text('mark'));
  const point = append(root, text('point'));
  const { document, selection } = resetEnvironment(root);
  selection.setBaseAndExtent(mark, 0, point, 2);
  const caret = bareCaret(root);
  caret.markActive = true;
  caret._savedFocus = { node: point, offset: 2 };
  caret._cancelCaretTask = () => {};
  let clicked = null;
  const target = { dispatchEvent(event) { clicked = event; } };
  document.elementFromPoint = () => target;
  caret._cursorRectAt = (node) => node === point
    ? { top: 20, bottom: 30, left: 40, width: 10, height: 10 }
    : { top: 0, bottom: 10, left: 0, width: 10, height: 10 };

  caret.clickAtCaret();
  assert.equal(clicked.clientX, 45);
  assert.equal(clicked.clientY, 25);
});

test('window scrolling is explicitly instant when distance is read synchronously', () => {
  const { root } = resetEnvironment();
  const caret = bareCaret(root);
  let options = null;
  context.window.scrollBy = (next) => {
    options = next;
    context.window.scrollY += next.top;
  };

  assert.equal(caret._scrollBy(120), 120);
  assert.equal(options.top, 120);
  assert.equal(options.left, 0);
  assert.equal(options.behavior, 'instant');
});

test('hash success starts reveal instead of only redrawing point', () => {
  const root = element('BODY');
  const target = append(root, element('H2', 'target'));
  append(target, text('Target'));
  const { document } = resetEnvironment(root);
  const caret = bareCaret(root);
  const task = caret._startCaretTask({ hash: 'target', deadline: 1000 });
  document.getElementById = () => target;
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._textVisibleEdgeOffset = () => 0;
  let reveals = 0;
  caret._revealCaret = () => { reveals += 1; };

  caret._pollHashTarget(task);
  assert.equal(reveals, 1);
});

test('existing empty fragment targets resolve following text immediately', () => {
  const root = element('BODY');
  const anchor = append(root, element('A', 'chapter'));
  const heading = append(root, element('H2'));
  const headingText = append(heading, text('Chapter'));
  const { document, selection } = resetEnvironment(root);
  const caret = bareCaret(root);
  document.getElementById = () => anchor;
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._textVisibleEdgeOffset = () => 0;
  caret._walkToVisible = (node, fwd) => node === anchor && fwd ? headingText : null;
  caret._revealCaret = () => {};

  assert.equal(caret._restoreHashTarget('chapter'), true);
  assert.equal(selection.focusNode, headingText);
});

test('missing fragment timeout falls back to ordinary restoration', () => {
  const { root, document } = resetEnvironment();
  const clock = installClock();
  const caret = bareCaret(root);
  document.getElementById = () => null;
  let fallbacks = 0;
  caret._restoreDefaultCaret = () => { fallbacks += 1; };

  assert.equal(caret._restoreHashTarget('missing'), true);
  clock.runTimers();
  assert.equal(fallbacks, 1);
});

test('resize without a bookmark retains pending hash restoration', () => {
  const root = element('BODY');
  const { document, selection } = resetEnvironment(root);
  const clock = installClock();
  const caret = deferredInstance(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._textVisibleEdgeOffset = () => 0;
  let target = null;
  document.getElementById = () => target;

  assert.equal(caret._restoreHashTarget('late'), true);
  caret._onResize();
  target = append(root, element('H2', 'late'));
  const targetText = append(target, text('Late'));
  clock.runTimers();
  assert.equal(selection.focusNode, targetText);
});

test('resize always resumes persisted-offset tracking after a DOM-path match', () => {
  const root = element('BODY');
  const node = append(root, text('point'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(node, 2);
  const caret = deferredInstance(root);
  caret._savedFocus = { node, offset: 2 };
  caret._caretStorageKey = () => 'caret';
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._selectionFocusRect = () => ({ top: 0, bottom: 10, height: 10 });
  caret._isRectInViewport = () => true;
  caret._updateCursor = () => {};
  storage.set('caret', '2');
  let resumedBookmark = null;
  caret._beginStoredRestore = (target, deadline, bookmark) => { resumedBookmark = bookmark; };

  assert.equal(caret._restoreCaret(), true);
  caret._onResize();
  clock.runNextTimer();
  assert.equal(resumedBookmark.globalOffset, 2);
  assert.equal(resumedBookmark.node, undefined);
  assert.equal(resumedBookmark.path, undefined);
});

test('marked movement clamps stale saved-focus offsets after DOM shrink', () => {
  const root = element('BODY');
  const node = append(root, text('short'));
  const { selection } = resetEnvironment(root);
  selection.setBaseAndExtent(node, 0, node, node.length);
  const caret = bareCaret(root);
  caret.markActive = true;
  caret._markAnchor = { node, offset: 0 };
  caret._savedFocus = { node, offset: 20 };
  caret._cancelCaretTask = () => {};
  caret._ensureSelection = () => selection;
  caret._isAtVisibleBoundary = () => true;

  assert.doesNotThrow(() => caret._moveCaret('forward', 'character'));
  assert.equal(selection.focusOffset, node.length);
});

test('deactivateMark clamps stale saved-caret offsets after DOM shrink', () => {
  const root = element('BODY');
  const node = append(root, text('short'));
  const { selection } = resetEnvironment(root);
  selection.collapse(node, 1);
  const caret = bareCaret(root);
  caret.markActive = true;
  caret._savedCaret = { node, offset: 20 };
  caret._updateCursor = () => {};

  assert.doesNotThrow(() => caret.deactivateMark());
  assert.equal(selection.focusOffset, node.length);
});

test('marked resize preserves logical saved focus instead of extended endpoint', () => {
  const root = element('BODY');
  const mark = append(root, element('P', 'mark'));
  const point = append(root, element('P', 'point'));
  const markText = append(mark, text('mark'));
  const pointText = append(point, text('point'));
  const { selection } = resetEnvironment(root);
  selection.setBaseAndExtent(markText, 0, pointText, 4);
  const caret = bareCaret(root);
  caret.markActive = true;
  caret._markAnchor = { node: markText, offset: 0 };
  caret._savedCaret = { node: pointText, offset: 2 };
  caret._savedFocus = { node: pointText, offset: 2 };
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;

  const snapshot = caret._captureResizeRestore(null);
  assert.equal(snapshot.point.node, pointText);
  assert.equal(snapshot.point.offset, 2);
});

test('stored offset records which side of a text-node boundary was point', () => {
  const root = element('BODY');
  const previous = append(root, text('foo'));
  const following = append(root, text('bar'));
  const { storage } = resetEnvironment(root);
  const caret = bareCaret(root);
  caret._caretStorageKey = () => 'caret';
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._caretGlobalOffset = (node, offset) => node === following ? 3 + offset : offset;
  caret.markActive = true;
  caret._savedFocus = { node: following, offset: 0 };
  caret._saveCaretPosition();
  const stored = JSON.parse(storage.get('caret'));
  assert.equal(stored.offset, previous.length);
  assert.equal(stored.nextBoundary, true);
  assert.equal(caret._resolveTextBookmark(
    { globalOffset: stored.offset, nextBoundary: stored.nextBoundary }).node, following);
});

test('PDF fallback compares candidates across visible pages', () => {
  const root = element('DIV');
  const first = element('DIV');
  const second = element('DIV');
  const { document } = resetEnvironment(root);
  const makePage = (top, label) => {
    const node = text(label);
    const span = {
      firstChild: node,
      getBoundingClientRect: () => ({ top, bottom: top + 10, left: 10, width: 40, height: 10 }),
    };
    return {
      getBoundingClientRect: () => ({ top, bottom: top + 100, left: 0, right: 800 }),
      querySelector: () => ({ querySelectorAll: () => [span] }),
    };
  };
  const upper = makePage(0, 'upper');
  const lower = makePage(80, 'lower');
  const caret = bareCaret(root);
  caret.el = { querySelectorAll: () => [upper, lower] };
  caret._isNavigableTextNode = () => true;
  caret._textVisibleEdgeOffset = () => 0;
  document.createRange = () => makeRange();
  const range = caret._findVisibleText(false, { top: 0, bottom: 200 }, null);
  assert.equal(range.startContainer.textContent, 'lower');
});

test('element-boundary resolution includes its direct text child', () => {
  const root = element('BODY');
  const paragraph = append(root, element('P'));
  const first = append(paragraph, text('first'));
  append(paragraph, append(element('EM'), text('second')));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;

  const point = caret._resolveCursorPosition(paragraph, 0);
  assert.equal(point.node, first);
  const end = caret._resolveCursorPosition(paragraph, paragraph.childNodes.length);
  assert.equal(end.node.textContent, 'second');
  assert.equal(end.offset, 'second'.length);
});
test('PDF buffer edge uses the page at the post-scroll viewport edge', () => {
  const root = element('DIV');
  const { document } = resetEnvironment(root);
  const caret = bareCaret(root);
  caret.scrollContainer = {
    scrollTop: 0, scrollHeight: 1000, clientHeight: 300,
    getBoundingClientRect: () => ({ top: 0, bottom: 300, left: 0, right: 800 }),
  };
  const textLayer = { children: [{}] };
  const destination = {
    dataset: { pageNumber: '1' },
    querySelector: () => textLayer,
  };
  document.elementFromPoint = () => ({ closest: () => destination });
  caret._scrollTo = () => {};
  let jumped = null;
  caret._jumpToEdge = (toStart, page) => { jumped = { toStart, page }; };

  caret._jumpToBufferEdge(true);
  assert.equal(jumped.toStart, true);
  assert.equal(jumped.page, destination);
});

test('expanded HTML selection renders mark-to-point direction', () => {
  const root = element('BODY');
  const sentence = append(root, text('One sentence.'));
  const { selection } = resetEnvironment(root);
  selection.setBaseAndExtent(sentence, 0, sentence, 3);
  const caret = bareCaret(root);
  caret.scrollContainer = null;
  caret._detectSelectionScope = () => 'word';
  caret._resolveCursorPosition = (node, offset) => ({ node, offset });
  selection.modify = (alter, direction) => {
    if (direction === 'backward') selection.setBaseAndExtent(sentence, 0, sentence, 3);
    else selection.setBaseAndExtent(sentence, 0, sentence, sentence.length);
  };
  caret._renderedRangeEndpoints = () => ({
    startNode: sentence, startOffset: 0, endNode: sentence, endOffset: sentence.length,
  });

  caret.expandSelection();
  assert.equal(selection.anchorOffset, 0);
  assert.equal(selection.focusOffset, sentence.length);
  assertPoint(caret._markAnchor, sentence, 0);
  assertPoint(caret._savedFocus, sentence, sentence.length);
});

test('word expansion at a trailing terminator excludes the terminator', () => {
  const root = element('BODY');
  const node = append(root, text('First sentence. '));
  const { selection } = resetEnvironment(root);
  selection.collapse(node, node.length);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;

  caret.expandSelection();
  assert.equal(caret.markActive, true);
  assertPoint(caret._markAnchor, node, 6);
  assertPoint(caret._savedFocus, node, 14);
  assert.equal(selection.anchorOffset, 6);
  assert.equal(selection.focusOffset, 14);
});

test('sentence expansion from the last word keeps the current sentence', () => {
  const root = element('BODY');
  const node = append(root, text('One sentence. Two more.'));
  const { selection } = resetEnvironment(root);
  selection.setBaseAndExtent(node, 4, node, 12);
  const caret = bareCaret(root);
  caret.scrollContainer = null;
  caret._renderedRangeEndpoints = () => null;
  caret._resolveCursorPosition = (current, offset) => ({ node: current, offset });
  selection.getRangeAt = () => ({ startContainer: node, startOffset: 4 });
  const terminator = 12; // index of the '.' ending the first sentence
  selection.modify = (_alter, direction) => {
    const from = selection.focusOffset;
    if (direction === 'backward') {
      selection.focusOffset = 0;
    } else {
      // WebKit: a reference at/after the terminator belongs to the next
      // sentence, so the forward boundary is that sentence's end.
      selection.focusOffset = from > terminator ? node.length : 13;
    }
  };

  caret._expandTo(selection, 'sentenceboundary');
  assert.equal(selection.anchorOffset, 0);
  assert.equal(selection.focusOffset, 13);
  assertPoint(caret._markAnchor, node, 0);
  assertPoint(caret._savedFocus, node, 13);
});

test('cross-page movement re-reads lines after the page scrolls into view', () => {
  const root = element('DIV');
  const currentPage = append(root, element('DIV'));
  const nextPage = append(root, element('DIV'));
  const targetText = append(nextPage, text('word'));
  const { document } = resetEnvironment(root);
  let nextTop = 900;
  currentPage.getBoundingClientRect = () => ({ top: 100, bottom: 800, left: 0, right: 600, width: 600, height: 700 });
  nextPage.getBoundingClientRect = () => ({ top: nextTop, bottom: nextTop + 700, left: 0, right: 600, width: 600, height: 700 });
  nextPage.scrollIntoView = () => { nextTop = 0; };
  root.querySelectorAll = () => [currentPage, nextPage];
  const charRect = () => ({ top: nextTop + 5, bottom: nextTop + 17, left: 40, right: 70, width: 30, height: 12 });
  const caret = bareCaret(root);
  caret.scrollContainer = {
    getBoundingClientRect: () => ({ top: 0, bottom: 600, left: 0, right: 800, width: 800, height: 600 }),
  };
  caret._visuallyOrderedTextNodes = (scopeRoot) => scopeRoot === nextPage
    ? { ordered: [1], lines: [[{ node: targetText, rect: charRect(), startOffset: 0, endOffset: 4 }]] }
    : { ordered: [], lines: [] };
  document.createRange = () => {
    const range = makeRange();
    range.getBoundingClientRect = function () {
      return this.startContainer === targetText ? charRect()
        : { top: 0, bottom: 10, left: 0, right: 1, width: 1, height: 10 };
    };
    return range;
  };

  const range = caret._moveLineCrossPage(currentPage, true, 55);
  assert.equal(range?.startContainer, targetText);
  assert.equal(range.startOffset, 0);
});

test('caretInfo returns zeros without a rendered caret position', () => {
  const root = element('BODY');
  resetEnvironment(root);
  const caret = bareCaret(root);

  assert.deepEqual(JSON.parse(caret.caretInfo()), { text: '', left: 0, bottom: 0 });
});

test('word expansion from an element caret keeps mark at the word start', () => {
  const root = element('BODY');
  const paragraph = append(root, element('P'));
  const word = append(paragraph, text('abc'));
  const { selection } = resetEnvironment(root);
  selection.collapse(paragraph, 0);
  const caret = bareCaret(root);
  selection.modify = (alter) => {
    if (alter === 'move') selection.collapse(word, 3);
    else selection.setBaseAndExtent(word, 3, word, 0);
  };

  caret.expandSelection();
  assert.equal(caret.markActive, true);
  assertPoint(caret._markAnchor, word, 0);
  assertPoint(caret._savedFocus, word, 3);
  assert.equal(selection.anchorOffset, 0);
  assert.equal(selection.focusOffset, 3);
});

test('boundary check reports only the dispatched command outcome', () => {
  const root = element('BODY');
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._lastMoved = false;
  caret._lastDir = 'forward';
  caret.pageDown = () => {};
  caret.isAtBottom = () => true;

  assert.equal(caret.moveWithBoundaryCheck('pageDown'), 'ok');
});

test('SPA route fragments skip the hash target poll', () => {
  const root = element('BODY');
  append(root, text('content'));
  resetEnvironment(root);
  context.location.hash = '#/route';
  const caret = bareCaret(root);
  caret._caretStorageKey = () => 'caret';

  caret.restore();
  assert.ok(!caret._caretTask?.hash);
  caret._cancelCaretTask();
});

test('persisted restore keeps tracking through the hydration deadline', () => {
  const root = element('BODY');
  const placeholder = append(root, text('placeholder'));
  const final = append(root, text('final'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  const caret = bareCaret(root);
  storage.set('caret', '2');
  caret._caretStorageKey = () => 'caret';
  caret._resolveGlobalOffset = () => clock.now >= 300
    ? { node: final, offset: 2 } : { node: placeholder, offset: 2 };
  caret._hasRenderedBox = () => true;
  caret._selectionFocusRect = () => ({ top: 10, bottom: 20, height: 10 });
  caret._isRectInViewport = () => true;
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });

  caret._restoreCaret();
  clock.runTimers();
  assert.equal(selection.focusNode, final);
});

test('resize preserves stored text-boundary side metadata', () => {
  const root = element('BODY');
  const previous = append(root, text('foo'));
  const following = append(root, text('bar'));
  const { selection, storage } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(following, 0);
  const caret = deferredInstance(root);
  caret._savedFocus = { node: following, offset: 0 };
  caret._caretStorageKey = () => 'caret';
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  caret._caretGlobalOffset = (node, offset) => node === following ? previous.length + offset : offset;
  storage.set('caret', JSON.stringify({ offset: previous.length, nextBoundary: true }));
  let boundary = null;
  caret._beginStoredRestore = (target, deadline, bookmark) => { boundary = bookmark.nextBoundary; };

  caret._restoreCaret();
  caret._onResize();
  clock.runNextTimer();
  assert.equal(boundary, true);
});

test('PDF retry replaces an obsolete pending page listener', () => {
  const root = element('DIV');
  const node = append(root, text('point'));
  const { selection } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(node, 0);
  const caret = bareCaret(root);
  caret.scrollContainer = {
    scrollTop: 10, scrollHeight: 1000, clientHeight: 200,
    getBoundingClientRect: () => ({ top: 0, bottom: 200, left: 0, right: 800 }),
  };
  caret._lastScrollTop = 0;
  caret._rangeRectAt = () => ({ top: 300, bottom: 310, left: 10, width: 1, height: 10 });
  caret._probeTextAt = () => null;
  caret._findVisibleText = () => null;
  caret._updateCursor = () => {};
  let cancelled = 0;
  const pages = [];
  caret._pendingTextRetry = { pageNumber: 1, cancel: () => { cancelled += 1; } };
  caret._onTextLayerReady = (callback, pageNumber) => {
    pages.push(pageNumber);
    return () => {};
  };
  context.window.PDFViewerApplication = { page: 2 };

  caret._onUserScroll();
  clock.runFrames();
  assert.equal(cancelled, 1);
  assert.deepEqual(pages, [2]);
});

test('probe fallback rejects a range outside the viewport', () => {
  const root = element('BODY');
  const node = append(root, text('point'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  const range = { startContainer: node, startOffset: 0 };
  caret._probeTextAt = () => range;
  caret._rangeRectAt = () => ({ top: 200, bottom: 210, width: 1, height: 10 });
  caret._viewportRect = () => ({ top: 0, bottom: 100, left: 0, right: 800 });
  assert.equal(caret._probeWithFallback(10, 10, true), null);
});

test('viewport resize suppresses relocation only when it actually scrolls', () => {
  const root = element('DIV');
  const node = append(root, text('point'));
  const { selection } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(node, 0);
  const caret = bareCaret(root);
  caret._caretViewportTop = 10;
  caret._selectionFocusRect = () => ({ top: 10, bottom: 20, height: 10 });
  caret._viewportRect = () => ({ top: 0, bottom: 100 });
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });
  caret._scheduleViewportResize();
  clock.runTimers();
  assert.equal(caret._suppressScrollRelocate, false);
});

test('canceling no task preserves independent scroll suppression', () => {
  const { root } = resetEnvironment();
  const caret = bareCaret(root);
  caret._suppressScrollRelocate = true;
  caret._cancelCaretTask();
  assert.equal(caret._suppressScrollRelocate, true);
});

test('reveal adopts a valid live selection when saved focus is absent', () => {
  const root = element('BODY');
  const node = append(root, text('point'));
  const { selection } = resetEnvironment(root);
  selection.collapse(node, 2);
  const caret = bareCaret(root);
  caret._hasRenderedBox = () => true;
  caret._selectionFocusRect = () => ({ top: 10, bottom: 20, height: 10 });
  caret._isRectInViewport = () => true;
  caret._updateCursor = () => {};
  Object.defineProperty(caret, '_scrollTop', { get: () => 0 });
  const task = caret._startCaretTask({ deadline: 1000 });
  caret._pollReveal(task);
  assertPoint(caret._savedFocus, node, 2);
});

test('element-end resolution uses the last rendered descendant', () => {
  const root = element('BODY');
  const paragraph = append(root, element('P'));
  const span = append(paragraph, element('SPAN'));
  append(span, text('first'));
  const emphasis = append(span, element('EM'));
  const last = append(emphasis, text('last'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = () => true;
  const point = caret._resolveCursorPosition(paragraph, paragraph.childNodes.length);
  assert.equal(point.node, last);
  assert.equal(point.offset, last.length);
});

test('element-boundary resolution skips hidden direct text', () => {
  const root = element('BODY');
  const paragraph = append(root, element('P'));
  const hidden = append(paragraph, text('hidden'));
  const visible = append(paragraph, text('visible'));
  resetEnvironment(root);
  const caret = bareCaret(root);
  caret._isNavigableTextNode = () => true;
  caret._hasRenderedBox = (node) => node !== hidden;
  caret._walkToVisible = () => visible;
  const point = caret._resolveCursorPosition(paragraph, 0);
  assert.equal(point.node, visible);
});

test('PDF retry subscribes to the page under the failed probe', () => {
  const root = element('DIV');
  const node = append(root, text('point'));
  const { document, selection } = resetEnvironment(root);
  const clock = installClock();
  selection.collapse(node, 0);
  const caret = bareCaret(root);
  caret.scrollContainer = {
    scrollTop: 10, scrollHeight: 1000, clientHeight: 200,
    getBoundingClientRect: () => ({ top: 0, bottom: 200, left: 0, right: 800 }),
  };
  caret._lastScrollTop = 0;
  caret._rangeRectAt = () => ({ top: 300, bottom: 310, left: 10, width: 1, height: 10 });
  caret._probeTextAt = () => null;
  caret._findVisibleText = () => null;
  caret._updateCursor = () => {};
  document.elementFromPoint = () => ({
    closest: () => ({ dataset: { pageNumber: '4' } }),
  });
  context.window.PDFViewerApplication = { page: 3 };
  let subscribedPage = null;
  caret._onTextLayerReady = (callback, pageNumber) => {
    subscribedPage = pageNumber;
    return () => {};
  };
  caret._onUserScroll();
  clock.runFrames();
  assert.equal(subscribedPage, 4);
});

test('native top fragment is not replaced by stored caret restoration', () => {
  const { root, document } = resetEnvironment();
  const caret = bareCaret(root);
  document.getElementById = () => null;
  document.getElementsByName = () => [];
  let jumped = 0;
  let restored = 0;
  caret._jumpToEdge = (toStart) => { if (toStart) jumped += 1; };
  caret._restoreDefaultCaret = () => { restored += 1; };
  const task = caret._startCaretTask({ hash: 'top', deadline: 1000 });
  caret._pollHashTarget(task);
  assert.equal(jumped, 1);
  assert.equal(restored, 0);
});

test('PDF resize preserves a pending buffer-edge readiness task', () => {
  const { root } = resetEnvironment();
  const caret = bareCaret(root);
  const task = caret._startCaretTask({ unsubscribe: () => {} });
  caret._caretViewportTop = 10;
  caret._scheduleViewportResize();
  assert.equal(caret._caretTask, task);
});

test('resize waits for delayed pre-expansion saved caret', () => {
  const root = element('BODY');
  const point = append(root, text('point'));
  const mark = append(root, text('mark'));
  const { selection } = resetEnvironment(root);
  const clock = installClock();
  selection.setBaseAndExtent(mark, 0, point, 2);
  const caret = bareCaret(root);
  const saved = {
    point: { node: point, offset: 2 },
    mark: { node: mark, offset: 0 },
    savedCaret: { globalOffset: 10, offset: 1 },
    markActive: true,
    viewportTop: null,
  };
  const task = caret._startCaretTask({ snapshot: saved, deadline: 1000 });
  let attempts = 0;
  caret._resolveTextBookmark = (bookmark) => {
    if (bookmark === saved.savedCaret) return ++attempts > 1 ? { node: point, offset: 1 } : null;
    return bookmark;
  };
  caret._selectionFocusRect = () => null;
  caret._revealCaret = () => {};
  caret._restoreResizedSelection(task);
  assert.equal(caret._savedCaret, null);
  clock.runNextTimer();
  assertPoint(caret._savedCaret, point, 1);
});

test('clickAtCaret falls back to live point when saved marked point detaches', () => {
  const root = element('BODY');
  const detached = text('detached');
  const live = append(root, text('live'));
  const { document, selection } = resetEnvironment(root);
  selection.collapse(live, 2);
  const caret = bareCaret(root);
  caret.markActive = true;
  caret._savedFocus = { node: detached, offset: 2 };
  let clicked = false;
  document.elementFromPoint = () => ({ dispatchEvent() { clicked = true; } });
  caret._cursorRectAt = () => ({ top: 0, bottom: 10, left: 0, width: 10, height: 10 });
  caret.clickAtCaret();
  assert.equal(clicked, true);
});
