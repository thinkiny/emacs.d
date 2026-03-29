#!/usr/bin/env python3
"""Synchronize generated EPUB and PDF.js reader assets with theme colors."""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence


REPO_ROOT = Path(__file__).resolve().parent.parent
NOV_STOCK_LIGHT_CSS = REPO_ROOT / "assets" / "css" / "nov-light.css"
NOV_STOCK_DARK_CSS = REPO_ROOT / "assets" / "css" / "nov-dark.css"
NOV_OVERRIDE_CSS = REPO_ROOT / "assets" / "css" / "nov-override.css"
PDFJS_VIEWER_HTML = REPO_ROOT / "assets" / "pdfjs" / "web" / "viewer.html"
PDFJS_VIEWER_CSS = REPO_ROOT / "assets" / "pdfjs" / "web" / "viewer.css"

NOV_MARKER_START = "/* reader-color-override:nov:start */"
NOV_MARKER_END = "/* reader-color-override:nov:end */"
PDFJS_THEME_MARKER_START = "<!-- reader-color-override:pdfjs-theme:start -->"
PDFJS_THEME_MARKER_END = "<!-- reader-color-override:pdfjs-theme:end -->"
PDFJS_CSS_MARKER_START = "/* reader-color-override:pdfjs:start */"
PDFJS_CSS_MARKER_END = "/* reader-color-override:pdfjs:end */"
PDFJS_THEME_HOOK_ANCHOR = '  <script src="viewer.mjs" type="module"></script>'

NOV_TEMPLATE = """html,
body,
body > * ,
section,
main,
article,
div,
p,
span,
pre,
code,
blockquote,
dl,
dt,
dd,
ol,
ul,
li,
table,
thead,
tbody,
tr,
th,
td,
figure,
figcaption {{
  background: {background} !important;
  background-color: {background} !important;
}}

html,
body {{
  color: {foreground} !important;
}}
"""

PDFJS_THEME_TEMPLATE = """<script>
document.addEventListener(
  "webviewerloaded",
  () => {{
    if (window.PDFViewerApplicationOptions) {{
      window.PDFViewerApplicationOptions.set("viewerCssTheme", {viewer_css_theme});
      window.PDFViewerApplicationOptions.set("transparentPageBackground", true);
    }}
  }},
  {{ once: true }}
);
</script>"""

PDFJS_CHROME_TEMPLATE = """:root {{
  --page-bg-color: {background};
  --page-fg-color: {foreground};
  --body-bg-color: {background};
  --toolbar-bg-color: {background};
  --sidebar-toolbar-bg-color: {background};
  --sidebar-narrow-bg-color: {background};
  --doorhanger-bg-color: {background};
  --sidebar-bg-color: {background};
  --main-color: {foreground};
  --text-primary-color: {foreground};
  --text-secondary-color: {foreground};
  --toolbar-icon-bg-color: {foreground};
  --toolbar-icon-hover-bg-color: {foreground};
  --field-color: {foreground};
  --field-bg-color: {background};
  --input-text-bg-color: {background};
  --input-text-fg-color: {foreground};
  --textarea-bg-color: {background};
  --textarea-fg-color: {foreground};
  --dialog-bg-color: {background};
  --dialog-border-color: {background};
}}

body,
#outerContainer,
#mainContainer,
#viewerContainer {{
  background-color: var(--body-bg-color);
  color: var(--text-primary-color);
}}

#sidebarContainer {{
  background-color: var(--sidebar-narrow-bg-color);
  color: var(--text-primary-color);
}}

#toolbarContainer,
#toolbarSidebar,
#secondaryToolbar,
#findbar,
.toolbarSlot,
.toolbar {{
  background-color: var(--toolbar-bg-color);
  color: var(--text-primary-color);
}}

.sidebar {{
  background-color: var(--sidebar-bg-color);
  color: var(--text-primary-color);
}}

#viewsManager {{
  --sidebar-bg-color: var(--body-bg-color);
  --header-bg: var(--toolbar-bg-color);
  --status-actions-bg: var(--toolbar-bg-color);
  --status-undo-bg: var(--toolbar-bg-color);
  --status-warning-bg: var(--toolbar-bg-color);
  --status-waiting-bg: var(--toolbar-bg-color);
  background-color: var(--sidebar-bg-color);
  color: var(--text-primary-color);
}}

#viewsManager #viewsManagerHeader,
#viewsManager #viewsManagerContent,
#outlinesView,
#attachmentsView,
#layersView,
#thumbnailsView {{
  background-color: var(--sidebar-bg-color);
  color: var(--text-primary-color);
}}

#secondaryToolbar,
.doorHanger,
.doorHangerRight {{
  background-color: var(--doorhanger-bg-color);
  color: var(--text-primary-color);
}}

input,
select,
textarea {{
  background-color: var(--input-text-bg-color);
  color: var(--input-text-fg-color);
}}
"""


@dataclass(frozen=True)
class ThemeMode:
    name: str
    pdfjs_viewer_css_theme: int


THEME_MODES: dict[str, ThemeMode] = {
    "light": ThemeMode(name="light", pdfjs_viewer_css_theme=1),
    "dark": ThemeMode(name="dark", pdfjs_viewer_css_theme=2),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Update generated EPUB/PDF.js reader assets to match a provided theme mode and theme colors."
    )
    parser.add_argument(
        "--restore-stock-light-assets",
        action="store_true",
        help="Rebuild generated EPUB CSS from stock light CSS and restore stock light PDF.js assets.",
    )
    parser.add_argument(
        "theme",
        nargs="?",
        choices=sorted(THEME_MODES),
        help="Theme mode for generated overrides.",
    )
    parser.add_argument(
        "foreground",
        nargs="?",
        help="Foreground hex color (e.g. '#f8f8f2' or 'f8f8f2'). Three-digit forms are also accepted.",
    )
    args = parser.parse_args()
    if args.restore_stock_light_assets:
        if args.theme or args.foreground:
            parser.error(
                "--restore-stock-light-assets does not accept theme or foreground arguments."
            )
    elif not args.theme or not args.foreground:
        parser.error(
            "theme and foreground are required unless --restore-stock-light-assets is used."
        )
    return args


def normalize_hex_color(value: str) -> str:
    if not value:
        raise ValueError("Color value cannot be empty.")
    stripped = value.strip()
    if stripped.startswith("#"):
        stripped = stripped[1:]
    if len(stripped) not in (3, 6):
        raise ValueError("Color must have 3 or 6 hexadecimal digits.")
    if not re.fullmatch(r"[0-9a-fA-F]{%d}" % len(stripped), stripped):
        raise ValueError("Color must contain only hexadecimal digits.")
    if len(stripped) == 3:
        stripped = "".join(ch * 2 for ch in stripped)
    return f"#{stripped.lower()}"


def ensure_paths_exist(paths: Sequence[Path]) -> None:
    missing = [str(path) for path in paths if not path.exists()]
    if missing:
        raise FileNotFoundError(
            "Required file(s) not found:\n- " + "\n- ".join(missing)
        )


def build_marker_block(marker_start: str, marker_end: str, body: str) -> str:
    return f"{marker_start}\n{body.rstrip()}\n{marker_end}"


def remove_marker_block(contents: str, marker_start: str, marker_end: str) -> str:
    pattern = re.compile(
        r"\n*"
        + re.escape(marker_start)
        + r".*?"
        + re.escape(marker_end)
        + r"\n*",
        flags=re.DOTALL,
    )
    updated, count = pattern.subn("\n\n", contents)
    if count == 0:
        return contents
    return updated.rstrip() + "\n"


def set_marker_block(contents: str, marker_start: str, marker_end: str, body: str) -> str:
    block = build_marker_block(marker_start, marker_end, body)
    pattern = re.compile(
        re.escape(marker_start) + r".*?" + re.escape(marker_end),
        flags=re.DOTALL,
    )
    updated, count = pattern.subn(block, contents, count=1)
    if count == 0:
        trimmed = contents.rstrip()
        if trimmed:
            return f"{trimmed}\n\n{block}\n"
        return f"{block}\n"
    return updated


def read_text_if_exists(path: Path) -> str:
    if not path.exists():
        return ""
    return path.read_text(encoding="utf-8")


def write_text_if_changed(path: Path, updated: str) -> bool:
    contents = read_text_if_exists(path)
    if updated == contents:
        return False
    path.write_text(updated, encoding="utf-8")
    return True


def nov_stock_source(theme_mode: ThemeMode) -> Path:
    if theme_mode.name == "light":
        return NOV_STOCK_LIGHT_CSS
    return NOV_STOCK_DARK_CSS


def resolve_background(background: str, transparent_background: bool) -> str:
    return "transparent"


# EPUB rebuild helpers

def build_nov_override_body(background: str, foreground: str, *, transparent_background: bool = False) -> str:
    return NOV_TEMPLATE.format(
        background="transparent",
        foreground=foreground,
    )


def sanitize_nov_stock_contents(source_path: Path) -> str:
    contents = source_path.read_text(encoding="utf-8")
    return remove_marker_block(contents, NOV_MARKER_START, NOV_MARKER_END)


def rebuild_nov_override(
    theme_mode: ThemeMode,
    background: str,
    foreground: str,
    *,
    transparent_background: bool = False,
) -> bool:
    stock_contents = sanitize_nov_stock_contents(nov_stock_source(theme_mode))
    updated = set_marker_block(
        stock_contents,
        NOV_MARKER_START,
        NOV_MARKER_END,
        build_nov_override_body(background, foreground, transparent_background=True),
    )
    return write_text_if_changed(NOV_OVERRIDE_CSS, updated)


def restore_stock_light_nov_override() -> bool:
    updated = sanitize_nov_stock_contents(NOV_STOCK_LIGHT_CSS)
    return write_text_if_changed(NOV_OVERRIDE_CSS, updated)


# PDF.js patch helpers

def build_pdfjs_theme_hook(theme_mode: ThemeMode) -> str:
    return PDFJS_THEME_TEMPLATE.format(
        viewer_css_theme=theme_mode.pdfjs_viewer_css_theme,
    )


def build_pdfjs_chrome_override(
    background: str,
    foreground: str,
    *,
    transparent_background: bool = False,
) -> str:
    return PDFJS_CHROME_TEMPLATE.format(
        background="transparent",
        foreground=foreground,
    )


def insert_pdfjs_theme_hook(contents: str, body: str) -> str:
    cleaned = remove_marker_block(
        contents,
        PDFJS_THEME_MARKER_START,
        PDFJS_THEME_MARKER_END,
    )
    block = build_marker_block(
        PDFJS_THEME_MARKER_START,
        PDFJS_THEME_MARKER_END,
        body,
    )
    before, separator, after = cleaned.partition(PDFJS_THEME_HOOK_ANCHOR)
    if not separator:
        raise ValueError(f"Unable to find insertion anchor in {PDFJS_VIEWER_HTML}")
    return f"{before.rstrip()}\n\n{block}\n\n{PDFJS_THEME_HOOK_ANCHOR}{after}"


def sync_pdfjs_theme_hook(theme_mode: ThemeMode) -> bool:
    contents = PDFJS_VIEWER_HTML.read_text(encoding="utf-8")
    updated = insert_pdfjs_theme_hook(contents, build_pdfjs_theme_hook(theme_mode))
    return write_text_if_changed(PDFJS_VIEWER_HTML, updated)


def restore_pdfjs_theme_hook() -> bool:
    contents = PDFJS_VIEWER_HTML.read_text(encoding="utf-8")
    updated = remove_marker_block(
        contents,
        PDFJS_THEME_MARKER_START,
        PDFJS_THEME_MARKER_END,
    )
    return write_text_if_changed(PDFJS_VIEWER_HTML, updated)


def sync_pdfjs_chrome_override(
    background: str,
    foreground: str,
    *,
    transparent_background: bool = False,
) -> bool:
    contents = PDFJS_VIEWER_CSS.read_text(encoding="utf-8")
    updated = set_marker_block(
        contents,
        PDFJS_CSS_MARKER_START,
        PDFJS_CSS_MARKER_END,
        build_pdfjs_chrome_override(background, foreground, transparent_background=True),
    )
    return write_text_if_changed(PDFJS_VIEWER_CSS, updated)


def restore_pdfjs_chrome_override() -> bool:
    contents = PDFJS_VIEWER_CSS.read_text(encoding="utf-8")
    updated = remove_marker_block(
        contents,
        PDFJS_CSS_MARKER_START,
        PDFJS_CSS_MARKER_END,
    )
    return write_text_if_changed(PDFJS_VIEWER_CSS, updated)


# Top-level workflows

def restore_stock_light_reader_assets() -> int:
    try:
        ensure_paths_exist([NOV_STOCK_LIGHT_CSS, PDFJS_VIEWER_HTML, PDFJS_VIEWER_CSS])
    except FileNotFoundError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 3

    changed = False
    if restore_stock_light_nov_override():
        changed = True
        print(
            f"Restored {NOV_OVERRIDE_CSS.name} from stock light EPUB CSS ({NOV_STOCK_LIGHT_CSS.name})"
        )
    if restore_pdfjs_theme_hook():
        changed = True
        print("Restored stock light asset in pdf.js-theme-hook")
    if restore_pdfjs_chrome_override():
        changed = True
        print("Restored stock light asset in pdf.js-chrome-override")
    if not changed:
        print(
            "No changes needed; nov-override.css already matches stock light EPUB CSS "
            "and PDF.js reader assets are already restored"
        )
    return 0


def sync_reader_assets(
    theme_mode: ThemeMode,
    background: str,
    foreground: str,
    *,
    transparent_background: bool = False,
) -> int:
    stock_source = nov_stock_source(theme_mode)
    try:
        ensure_paths_exist([stock_source, PDFJS_VIEWER_HTML, PDFJS_VIEWER_CSS])
    except FileNotFoundError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 3

    changed = False
    if rebuild_nov_override(theme_mode, background, foreground, transparent_background=True):
        changed = True
        print(
            f"Rebuilt {NOV_OVERRIDE_CSS.name} from {stock_source.name} for {theme_mode.name} theme "
            f"with transparent background and foreground {foreground}"
        )
    if sync_pdfjs_theme_hook(theme_mode):
        changed = True
        print(
            f"Updated pdf.js-theme-hook for {theme_mode.name} theme with transparent background "
            f"and foreground {foreground}"
        )
    if sync_pdfjs_chrome_override(background, foreground, transparent_background=True):
        changed = True
        print(
            f"Updated pdf.js-chrome-override for {theme_mode.name} theme with transparent background "
            f"and foreground {foreground}"
        )
    if not changed:
        print(
            "No changes needed; EPUB/PDF.js reader assets already match "
            f"{theme_mode.name} theme with transparent background and foreground {foreground}"
        )
    return 0


def main() -> int:
    args = parse_args()

    if args.restore_stock_light_assets:
        return restore_stock_light_reader_assets()

    theme_mode = THEME_MODES[args.theme]
    try:
        foreground = normalize_hex_color(args.foreground)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    return sync_reader_assets(theme_mode, "", foreground, transparent_background=True)


if __name__ == "__main__":
    sys.exit(main())
