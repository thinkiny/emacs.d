#!/usr/bin/env python3
"""Synchronize generated EPUB and PDF.js reader assets with theme colors."""

import argparse
import re
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence
from zipfile import ZipFile


REPO_ROOT = Path(__file__).resolve().parent.parent
NOV_SOURCE_LIGHT_CSS = REPO_ROOT / "assets" / "css" / "nov-light.css"
NOV_SOURCE_DARK_CSS = REPO_ROOT / "assets" / "css" / "nov-dark.css"
NOV_OVERRIDE_CSS = REPO_ROOT / "assets" / "css" / "nov-override.css"
PDFJS_VIEWER_HTML = REPO_ROOT / "assets" / "pdfjs" / "web" / "viewer.html"
PDFJS_VIEWER_CSS = REPO_ROOT / "assets" / "pdfjs" / "web" / "viewer.css"
PDFJS_DIR = REPO_ROOT / "assets" / "pdfjs"
PDFJS_ZIP = REPO_ROOT / "assets" / "pdfjs.zip"

NOV_MARKER_START = "/* reader-color-override:nov:start */"
NOV_MARKER_END = "/* reader-color-override:nov:end */"
PDFJS_THEME_MARKER_START = "<!-- reader-color-override:pdfjs-theme:start -->"
PDFJS_THEME_MARKER_END = "<!-- reader-color-override:pdfjs-theme:end -->"
PDFJS_CSS_MARKER_START = "/* reader-color-override:pdfjs:start */"
PDFJS_CSS_MARKER_END = "/* reader-color-override:pdfjs:end */"
PDFJS_THEME_HOOK_ANCHOR = '  <script src="viewer.mjs" type="module"></script>'

NOV_TEMPLATE = """html,body{{
  background: {background} !important;
  color: {foreground} !important;
}}{dark_block}"""

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

PDFJS_UI_CSS_TEMPLATE = """:root {{
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
}}"""


@dataclass(frozen=True)
class AssetWrite:
    path: Path
    content: str
    message: str


# PDF.js ViewerCssThemeValues: LIGHT=1, DARK=2 (AUTOMATIC=0 unused).
PDFJS_VIEWER_CSS_THEME: dict[str, int] = {
    "light": 1,
    "dark": 2,
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Update generated EPUB/PDF.js reader assets to match a provided theme style and foreground color.",
        epilog=(
            "Examples:\n"
            "  python3 scripts/update_reader_theme_colors.py dark f8f8f2\n"
            "  python3 scripts/update_reader_theme_colors.py light '#202020'"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "theme_style",
        choices=sorted(PDFJS_VIEWER_CSS_THEME),
        help="Theme style for generated overrides.",
    )
    parser.add_argument(
        "--background",
        default="transparent",
        help="Background hex color for opaque mode (e.g. '#ffffff'). Default: transparent.",
    )
    parser.add_argument(
        "foreground",
        help="Foreground hex color (e.g. '#f8f8f2' or 'f8f8f2'). Three-digit forms are also accepted.",
    )
    args = parser.parse_args()
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


def extract_pdfjs() -> None:
    """Extract pdfjs.zip when it is newer than the extracted pdfjs directory."""
    if not PDFJS_ZIP.exists():
        return
    zip_mtime = PDFJS_ZIP.stat().st_mtime
    if PDFJS_DIR.exists() and PDFJS_DIR.stat().st_mtime >= zip_mtime:
        return
    if PDFJS_DIR.exists():
        shutil.rmtree(PDFJS_DIR)
    with ZipFile(PDFJS_ZIP, "r") as z:
        z.extractall(path=PDFJS_DIR.parent)
    print(f"Extracted {PDFJS_ZIP.name}")


def build_marker_block(marker_start: str, marker_end: str, body: str) -> str:
    return f"{marker_start}\n{body.rstrip()}\n{marker_end}"


def ensure_single_marker_block(
    contents: str, marker_start: str, marker_end: str
) -> None:
    start_count = contents.count(marker_start)
    end_count = contents.count(marker_end)
    if start_count != end_count:
        raise ValueError(
            f"Mismatched marker counts for {marker_start!r} and {marker_end!r}."
        )
    if start_count > 1:
        raise ValueError(f"Duplicate marker block found for {marker_start!r}.")


def remove_marker_block(contents: str, marker_start: str, marker_end: str) -> str:
    ensure_single_marker_block(contents, marker_start, marker_end)
    pattern = re.compile(
        r"\n*" + re.escape(marker_start) + r".*?" + re.escape(marker_end) + r"\n*",
        flags=re.DOTALL,
    )
    updated, count = pattern.subn("\n\n", contents)
    if count == 0:
        return contents
    return updated.rstrip() + "\n"


def set_marker_block(
    contents: str, marker_start: str, marker_end: str, body: str
) -> str:
    ensure_single_marker_block(contents, marker_start, marker_end)
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


def nov_source_path(theme_style: str) -> Path:
    if theme_style == "light":
        return NOV_SOURCE_LIGHT_CSS
    return NOV_SOURCE_DARK_CSS


def build_nov_override_body(theme_style: str, foreground: str, background: str) -> str:
    dark_block = ""
    if theme_style == "dark":
        dark_block = (
            "\n#sbo-rt-content * {{\n"
            "  color: {foreground} !important;\n"
            "  background: {background} !important;\n"
            "}}\n"
            "#sbo-rt-content img,\n"
            "#sbo-rt-content svg {{\n"
            "    filter: invert(1) hue-rotate(180deg) !important;\n"
            "}}\n"
        ).format(foreground=foreground, background=background)
    return NOV_TEMPLATE.format(
        foreground=foreground, background=background, dark_block=dark_block
    )


def strip_nov_generated_block(source_path: Path) -> str:
    contents = source_path.read_text(encoding="utf-8")
    return remove_marker_block(contents, NOV_MARKER_START, NOV_MARKER_END)


def build_nov_override_contents(
    theme_style: str, foreground: str, background: str
) -> str:
    source_contents = strip_nov_generated_block(nov_source_path(theme_style))
    return set_marker_block(
        source_contents,
        NOV_MARKER_START,
        NOV_MARKER_END,
        build_nov_override_body(theme_style, foreground, background),
    )


# PDF.js patch helpers


def build_pdfjs_theme_hook(viewer_css_theme: int) -> str:
    return PDFJS_THEME_TEMPLATE.format(viewer_css_theme=viewer_css_theme)


def build_pdfjs_chrome_override(foreground: str, background: str) -> str:
    return PDFJS_UI_CSS_TEMPLATE.format(foreground=foreground, background=background)


def insert_before_anchor(contents: str, anchor: str, block: str, path: Path) -> str:
    before, separator, after = contents.partition(anchor)
    if not separator:
        raise ValueError(f"Unable to find insertion anchor in {path}")
    return f"{before.rstrip()}\n\n{block}\n\n{anchor}{after}"


def build_pdfjs_theme_html_contents(viewer_css_theme: int) -> str:
    contents = PDFJS_VIEWER_HTML.read_text(encoding="utf-8")
    body = build_pdfjs_theme_hook(viewer_css_theme)
    if PDFJS_THEME_MARKER_START in contents:
        return set_marker_block(
            contents, PDFJS_THEME_MARKER_START, PDFJS_THEME_MARKER_END, body
        )
    block = build_marker_block(PDFJS_THEME_MARKER_START, PDFJS_THEME_MARKER_END, body)
    return insert_before_anchor(
        contents, PDFJS_THEME_HOOK_ANCHOR, block, PDFJS_VIEWER_HTML
    )


def build_pdfjs_viewer_css_contents(foreground: str, background: str) -> str:
    contents = PDFJS_VIEWER_CSS.read_text(encoding="utf-8")
    return set_marker_block(
        contents,
        PDFJS_CSS_MARKER_START,
        PDFJS_CSS_MARKER_END,
        build_pdfjs_chrome_override(foreground, background),
    )


def sync_reader_assets(theme_style: str, foreground: str, background: str) -> int:
    extract_pdfjs()
    source_css = nov_source_path(theme_style)
    try:
        ensure_paths_exist([source_css, PDFJS_VIEWER_HTML, PDFJS_VIEWER_CSS])
    except FileNotFoundError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 3

    viewer_css_theme = PDFJS_VIEWER_CSS_THEME[theme_style]
    changed = False
    try:
        operations = [
            AssetWrite(
                NOV_OVERRIDE_CSS,
                build_nov_override_contents(theme_style, foreground, background),
                f"Rebuilt {NOV_OVERRIDE_CSS.name} from {source_css.name} "
                f"for {theme_style} theme using background {background} and foreground {foreground}",
            ),
            AssetWrite(
                PDFJS_VIEWER_HTML,
                build_pdfjs_theme_html_contents(viewer_css_theme),
                f"Updated {PDFJS_VIEWER_HTML.name} for {theme_style} PDF.js theme hook",
            ),
            AssetWrite(
                PDFJS_VIEWER_CSS,
                build_pdfjs_viewer_css_contents(foreground, background),
                f"Updated {PDFJS_VIEWER_CSS.name} for {theme_style} PDF.js foreground {foreground}",
            ),
        ]
        for op in operations:
            if write_text_if_changed(op.path, op.content):
                print(op.message)
                changed = True
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 4

    if not changed:
        print(
            f"No changes needed; reader assets already match {theme_style} theme "
            f"with background {background} and foreground {foreground}"
        )
    return 0


def main() -> int:
    args = parse_args()

    try:
        foreground = normalize_hex_color(args.foreground)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    background = args.background
    if background != "transparent":
        try:
            background = normalize_hex_color(background)
        except ValueError as exc:
            print(f"error: {exc}", file=sys.stderr)
            return 2

    return sync_reader_assets(args.theme_style, foreground, background)


if __name__ == "__main__":
    sys.exit(main())
