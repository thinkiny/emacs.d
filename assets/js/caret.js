/**
 * CaretEmacs – Emacs-like caret navigation for WebKit.
 *
 * Keybindings:
 *   C-f       forward one character
 *   C-b       backward one character
 *   C-n       next line
 *   C-p       previous line
 *   C-e       end of line
 *   C-a       beginning of line
 *   C-v       page down
 *   M-v       page up
 *   C-Space   toggle mark (selection mode)
 *   C-g       cancel mark
 */

const PAGE_OVERLAP = 40; // pixels of overlap kept when paging, Emacs-style
const CURSOR_ID = "__caret-emacs-cursor";
const STYLE_ID  = "__caret-emacs-style";

class CaretEmacs {
  /**
   * @param {HTMLElement|Document} el  Element to listen on (default: document).
   * @param {object}              opts
   * @param {function}            opts.onMark  Called with (active: boolean) when mark toggles.
   */
  constructor(el = document, opts = {}) {
    this.el = el;
    this.onMark = opts.onMark || null;
    this.markActive = false;
    this._onKeyDown = this._handleKey.bind(this);
    this._onSelectionChange = this._updateCursor.bind(this);
    this._cursorEl = null;
    this._whenReady(() => {
      this._initCursor();
      this.enable();
      this._ensureSelection();
      this._updateCursor();
    });
  }

  /** Run a callback once the DOM body is available. */
  _whenReady(fn) {
    if (document.body) {
      fn();
    } else {
      document.addEventListener("DOMContentLoaded", fn, { once: true });
    }
  }

  /* ── lifecycle ────────────────────────────────────────────── */

  enable() {
    this.el.addEventListener("keydown", this._onKeyDown);
    document.addEventListener("selectionchange", this._onSelectionChange);
  }

  disable() {
    this.el.removeEventListener("keydown", this._onKeyDown);
    document.removeEventListener("selectionchange", this._onSelectionChange);
    this._clearMark();
  }

  destroy() {
    this.disable();
    this._removeCursor();
  }

  /* ── internals ───────────────────────────────────────────── */

  /**
   * Ensure a collapsed selection exists so that Selection.modify() has
   * something to work with.  On non-contenteditable nodes there is no
   * blinking caret by default, so the very first navigation key would
   * otherwise be a no-op.
   */
  _ensureSelection() {
    const sel = window.getSelection();
    if (sel && sel.rangeCount > 0) return sel;

    // Seed a collapsed selection at the start of the target element
    // (or <body> when listening on the document).
    const root =
      this.el === document ? document.body : this.el;

    const range = document.createRange();
    range.selectNodeContents(root);
    range.collapse(true); // collapse to start
    sel.removeAllRanges();
    sel.addRange(range);
    return sel;
  }

  _setMark(active) {
    this.markActive = active;
    if (!active) {
      // Collapse selection to the focus (cursor) end.
      const sel = window.getSelection();
      if (sel && sel.rangeCount) {
        sel.collapseToEnd();
      }
    }
    if (this.onMark) this.onMark(active);
  }

  _clearMark() {
    if (this.markActive) this._setMark(false);
  }

  /** Scroll the viewport if the selection focus point is off-screen. */
  _scrollToSelection() {
    const sel = window.getSelection();
    if (!sel || !sel.rangeCount) return;
    const r = document.createRange();
    r.setStart(sel.focusNode, sel.focusOffset);
    r.collapse(true);
    const rect = r.getBoundingClientRect();
    const margin = 40;
    if (rect.bottom > window.innerHeight - margin) {
      window.scrollBy(0, rect.bottom - window.innerHeight + margin);
    } else if (rect.top < margin) {
      window.scrollBy(0, rect.top - margin);
    }
  }

  /* ── visible cursor overlay ──────────────────────────────── */

  /** Inject a stylesheet + an absolutely-positioned cursor element. */
  _initCursor() {
    if (!document.getElementById(STYLE_ID)) {
      const style = document.createElement("style");
      style.id = STYLE_ID;
      style.textContent =
        "#" + CURSOR_ID + "{" +
          "position:fixed;width:2px;pointer-events:none;z-index:2147483647;" +
          "background:highlight;display:none;" +
          "animation:__caret-emacs-blink 1s step-end infinite" +
        "}" +
        "@keyframes __caret-emacs-blink{50%{opacity:0}}";
      document.head.appendChild(style);
    }

    if (!document.getElementById(CURSOR_ID)) {
      this._cursorEl = document.createElement("div");
      this._cursorEl.id = CURSOR_ID;
      // Append to <html> rather than <body> so the cursor is not affected
      // by transforms or absolute positioning on the body element.
      document.documentElement.appendChild(this._cursorEl);
    } else {
      this._cursorEl = document.getElementById(CURSOR_ID);
    }
  }

  /** Reposition the cursor overlay at the current selection focus point. */
  _updateCursor() {
    const el = this._cursorEl;
    if (!el) return;

    const sel = window.getSelection();
    if (!sel || !sel.rangeCount) { el.style.display = "none"; return; }

    const collapsed = document.createRange();
    collapsed.setStart(sel.focusNode, sel.focusOffset);
    collapsed.collapse(true);
    const rect = collapsed.getBoundingClientRect();

    if (!rect || !rect.height) { el.style.display = "none"; return; }

    el.style.display = "block";
    el.style.left    = rect.left + "px";
    el.style.top     = rect.top  + "px";
    el.style.height  = rect.height + "px";
  }

  /** Remove the cursor element and injected stylesheet. */
  _removeCursor() {
    const el = document.getElementById(CURSOR_ID);
    if (el) el.remove();
    const st = document.getElementById(STYLE_ID);
    if (st) st.remove();
    this._cursorEl = null;
  }

  /* ── key dispatch ────────────────────────────────────────── */

  _handleKey(e) {
    // C-<key>  →  ctrlKey (works on all platforms; macOS WebKit sends ctrlKey
    //             for the Control key even when Cmd is the "meta" key).
    // M-<key>  →  altKey  (Option on macOS, Alt elsewhere).

    if (e.ctrlKey && !e.altKey && !e.metaKey) {
      switch (e.key) {
        case "f":
          return this._move(e, "forward", "character");
        case "b":
          return this._move(e, "backward", "character");
        case "n":
          return this._move(e, "forward", "line");
        case "p":
          return this._move(e, "backward", "line");
        case "e":
          return this._move(e, "forward", "lineboundary");
        case "a":
          return this._move(e, "backward", "lineboundary");
        case "v":
          return this._page(e, "down");
        case " ":
          return this._toggleMark(e);
        case "g":
          return this._cancelMark(e);
        default:
          return; // not ours — let it propagate
      }
    }

    if (e.altKey && !e.ctrlKey && !e.metaKey) {
      switch (e.key) {
        case "v":
          return this._page(e, "up");
        default:
          return;
      }
    }
  }

  /* ── movement helpers ────────────────────────────────────── */

  _move(e, direction, granularity) {
    e.preventDefault();
    const sel = this._ensureSelection();
    if (!sel) return;

    if (this.markActive) {
      // Save the anchor (mark position), collapse to the focus (cursor),
      // move with "move" (more reliable than "extend" on non-editable
      // content in WebKit), then rebuild the selection.
      const an = sel.anchorNode, ao = sel.anchorOffset;
      sel.collapse(sel.focusNode, sel.focusOffset);
      sel.modify("move", direction, granularity);
      sel.setBaseAndExtent(an, ao, sel.focusNode, sel.focusOffset);
    } else {
      sel.modify("move", direction, granularity);
    }
    this._scrollToSelection();
  }

  /**
   * Page down / up.
   *
   * Strategy:
   *  1. Record the caret's x-position.
   *  2. Scroll the viewport by ±(innerHeight - overlap).
   *  3. Place the caret at the same x inside the new viewport using
   *     document.caretRangeFromPoint (WebKit-specific).
   *  4. When the mark is active, extend the selection instead of collapsing.
   */
  _page(e, direction) {
    e.preventDefault();

    const sel = this._ensureSelection();
    if (!sel || !sel.rangeCount) return;

    // 1. Record horizontal position of the caret.
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    const caretX = rect.left;

    // 2. Scroll.
    const delta = window.innerHeight - PAGE_OVERLAP;
    const scrollY = direction === "down" ? delta : -delta;
    window.scrollBy(0, scrollY);

    // 3. Compute the target y in viewport coordinates.
    //    After scrolling, the caret should sit at roughly the same viewport y
    //    it occupied before (it scrolled off, so we aim for the opposite edge).
    const targetY =
      direction === "down"
        ? PAGE_OVERLAP / 2           // near top after scrolling down
        : window.innerHeight - PAGE_OVERLAP / 2; // near bottom after scrolling up

    // 4. Re-position the caret.
    const range = document.caretRangeFromPoint(caretX, targetY);
    if (!range) return;

    if (this.markActive) {
      // Keep the anchor (mark), set focus to the new position.
      sel.setBaseAndExtent(
        sel.anchorNode, sel.anchorOffset,
        range.startContainer, range.startOffset
      );
    } else {
      sel.removeAllRanges();
      sel.addRange(range);
    }
  }

  _toggleMark(e) {
    e.preventDefault();
    this._setMark(!this.markActive);
  }

  _cancelMark(e) {
    e.preventDefault();
    if (this.markActive) this._setMark(false);
  }
}

// Support both ESM and plain <script> usage.
// When loaded as a plain <script>, auto-initialize on the document.
if (typeof exports !== "undefined") {
  exports.CaretEmacs = CaretEmacs;
} else if (typeof window !== "undefined") {
  window.CaretEmacs = CaretEmacs;
  new CaretEmacs(document);
}
