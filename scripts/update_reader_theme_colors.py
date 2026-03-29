#!/usr/bin/env python3
"""Synchronize generated EPUB and PDF.js reader assets with theme colors."""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Sequence


REPO_ROOT = Path(__file__).resolve().parent.parent
NOV_SOURCE_LIGHT_CSS = REPO_ROOT / "assets" / "css" / "nov-light.css"
NOV_SOURCE_DARK_CSS = REPO_ROOT / "assets" / "css" / "nov-dark.css"
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

NOV_TEMPLATE = """html,body {{
  background: transparent !important;
  background-color: transparent !important;
  color: {foreground} !important;
}}"""

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
  --page-bg-color: transparent;
  --page-fg-color: {foreground};
  --body-bg-color: transparent;
  --toolbar-bg-color: transparent;
  --sidebar-toolbar-bg-color: transparent;
  --sidebar-narrow-bg-color: transparent;
  --doorhanger-bg-color: transparent;
  --sidebar-bg-color: transparent;
  --main-color: {foreground};
  --text-primary-color: {foreground};
  --text-secondary-color: {foreground};
  --toolbar-icon-bg-color: {foreground};
  --toolbar-icon-hover-bg-color: {foreground};
  --field-color: {foreground};
  --field-bg-color: transparent;
  --input-text-bg-color: transparent;
  --input-text-fg-color: {foreground};
  --textarea-bg-color: transparent;
  --textarea-fg-color: {foreground};
  --dialog-bg-color: transparent;
  --dialog-border-color: transparent;
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
class ThemeMode:
    pdfjs_viewer_css_theme: int


@dataclass(frozen=True)
class AssetOperation:
    label: str
    path: Path
    build_expected: Callable[[], str]
    changed_message: str


THEME_MODES: dict[str, ThemeMode] = {
    "light": ThemeMode(pdfjs_viewer_css_theme=1),
    "dark": ThemeMode(pdfjs_viewer_css_theme=2),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Update generated EPUB/PDF.js reader assets to match a provided theme mode and foreground color.",
        epilog=(
            "Examples:\n"
            "  python3 scripts/update_reader_theme_colors.py dark f8f8f2\n"
            "  python3 scripts/update_reader_theme_colors.py light '#202020'\n"
            "  python3 scripts/update_reader_theme_colors.py --restore-stock-light-assets\n"
            "  python3 scripts/update_reader_theme_colors.py --check dark f8f8f2"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--restore-stock-light-assets",
        action="store_true",
        help="Rebuild generated EPUB CSS from light source CSS and remove generated PDF.js overrides.",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Validate the expected reader asset state without writing files.",
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


def ensure_single_marker_block(contents: str, marker_start: str, marker_end: str) -> None:
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


def nov_source_path(theme_name: str) -> Path:
    if theme_name == "light":
        return NOV_SOURCE_LIGHT_CSS
    return NOV_SOURCE_DARK_CSS


def build_nov_override_body(foreground: str) -> str:
    return NOV_TEMPLATE.format(foreground=foreground)


def strip_nov_generated_block(source_path: Path) -> str:
    contents = source_path.read_text(encoding="utf-8")
    return remove_marker_block(contents, NOV_MARKER_START, NOV_MARKER_END)


def build_nov_override_contents(theme_name: str, foreground: str) -> str:
    source_contents = strip_nov_generated_block(nov_source_path(theme_name))
    return set_marker_block(
        source_contents,
        NOV_MARKER_START,
        NOV_MARKER_END,
        build_nov_override_body(foreground),
    )


def build_restored_light_nov_override_contents() -> str:
    return strip_nov_generated_block(NOV_SOURCE_LIGHT_CSS)


# PDF.js patch helpers

def build_pdfjs_theme_hook(theme_mode: ThemeMode) -> str:
    return PDFJS_THEME_TEMPLATE.format(
        viewer_css_theme=theme_mode.pdfjs_viewer_css_theme,
    )


def build_pdfjs_chrome_override(foreground: str) -> str:
    return PDFJS_CHROME_TEMPLATE.format(foreground=foreground)


def insert_before_anchor(contents: str, anchor: str, block: str, path: Path) -> str:
    before, separator, after = contents.partition(anchor)
    if not separator:
        raise ValueError(f"Unable to find insertion anchor in {path}")
    return f"{before.rstrip()}\n\n{block}\n\n{anchor}{after}"


def build_pdfjs_theme_html_contents(theme_mode: ThemeMode) -> str:
    contents = PDFJS_VIEWER_HTML.read_text(encoding="utf-8")
    block = build_marker_block(
        PDFJS_THEME_MARKER_START,
        PDFJS_THEME_MARKER_END,
        build_pdfjs_theme_hook(theme_mode),
    )
    ensure_single_marker_block(
        contents,
        PDFJS_THEME_MARKER_START,
        PDFJS_THEME_MARKER_END,
    )
    if PDFJS_THEME_MARKER_START in contents:
        return set_marker_block(
            contents,
            PDFJS_THEME_MARKER_START,
            PDFJS_THEME_MARKER_END,
            build_pdfjs_theme_hook(theme_mode),
        )
    return insert_before_anchor(contents, PDFJS_THEME_HOOK_ANCHOR, block, PDFJS_VIEWER_HTML)


def build_restored_pdfjs_theme_html_contents() -> str:
    contents = PDFJS_VIEWER_HTML.read_text(encoding="utf-8")
    return remove_marker_block(
        contents,
        PDFJS_THEME_MARKER_START,
        PDFJS_THEME_MARKER_END,
    )


def build_pdfjs_viewer_css_contents(foreground: str) -> str:
    contents = PDFJS_VIEWER_CSS.read_text(encoding="utf-8")
    return set_marker_block(
        contents,
        PDFJS_CSS_MARKER_START,
        PDFJS_CSS_MARKER_END,
        build_pdfjs_chrome_override(foreground),
    )


def build_restored_pdfjs_viewer_css_contents() -> str:
    contents = PDFJS_VIEWER_CSS.read_text(encoding="utf-8")
    return remove_marker_block(
        contents,
        PDFJS_CSS_MARKER_START,
        PDFJS_CSS_MARKER_END,
    )


def apply_operation(operation: AssetOperation) -> bool:
    updated = operation.build_expected()
    return write_text_if_changed(operation.path, updated)


def check_operation(operation: AssetOperation) -> bool:
    expected = operation.build_expected()
    current = operation.path.read_text(encoding="utf-8")
    return current == expected


def run_operations(
    operations: Sequence[AssetOperation],
    *,
    check: bool,
    success_message: str,
    unchanged_message: str,
) -> int:
    changed_or_drifted = False
    for operation in operations:
        if check:
            if check_operation(operation):
                print(f"OK: {operation.path.name} already matches {operation.label}")
            else:
                changed_or_drifted = True
                print(f"DRIFT: {operation.path.name} does not match {operation.label}")
        elif apply_operation(operation):
            changed_or_drifted = True
            print(operation.changed_message)

    if check:
        if changed_or_drifted:
            print("Reader asset validation failed")
            return 1
        print(success_message)
        return 0

    if not changed_or_drifted:
        print(unchanged_message)
    return 0


def build_restore_operations() -> list[AssetOperation]:
    return [
        AssetOperation(
            label="light-source EPUB CSS",
            path=NOV_OVERRIDE_CSS,
            build_expected=build_restored_light_nov_override_contents,
            changed_message=(
                f"Rebuilt {NOV_OVERRIDE_CSS.name} from {NOV_SOURCE_LIGHT_CSS.name}"
            ),
        ),
        AssetOperation(
            label="stock viewer.html without generated PDF.js theme hook",
            path=PDFJS_VIEWER_HTML,
            build_expected=build_restored_pdfjs_theme_html_contents,
            changed_message=(
                f"Removed generated PDF.js theme hook from {PDFJS_VIEWER_HTML.name}"
            ),
        ),
        AssetOperation(
            label="stock viewer.css without generated PDF.js override block",
            path=PDFJS_VIEWER_CSS,
            build_expected=build_restored_pdfjs_viewer_css_contents,
            changed_message=(
                f"Removed generated PDF.js CSS override block from {PDFJS_VIEWER_CSS.name}"
            ),
        ),
    ]


def build_sync_operations(theme_name: str, foreground: str) -> list[AssetOperation]:
    theme_mode = THEME_MODES[theme_name]
    source_css = nov_source_path(theme_name)
    return [
        AssetOperation(
            label=f"{theme_name} EPUB reader override state",
            path=NOV_OVERRIDE_CSS,
            build_expected=lambda: build_nov_override_contents(theme_name, foreground),
            changed_message=(
                f"Rebuilt {NOV_OVERRIDE_CSS.name} from {source_css.name} "
                f"for {theme_name} theme using transparent background and foreground {foreground}"
            ),
        ),
        AssetOperation(
            label=f"{theme_name} PDF.js theme hook state",
            path=PDFJS_VIEWER_HTML,
            build_expected=lambda: build_pdfjs_theme_html_contents(theme_mode),
            changed_message=(
                f"Updated {PDFJS_VIEWER_HTML.name} for {theme_name} PDF.js theme hook"
            ),
        ),
        AssetOperation(
            label=f"{theme_name} PDF.js CSS override state",
            path=PDFJS_VIEWER_CSS,
            build_expected=lambda: build_pdfjs_viewer_css_contents(foreground),
            changed_message=(
                f"Updated {PDFJS_VIEWER_CSS.name} for {theme_name} PDF.js foreground {foreground}"
            ),
        ),
    ]


# Top-level workflows

def restore_stock_light_reader_assets(check: bool = False) -> int:
    try:
        ensure_paths_exist([NOV_SOURCE_LIGHT_CSS, PDFJS_VIEWER_HTML, PDFJS_VIEWER_CSS])
    except FileNotFoundError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 3
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 4

    try:
        return run_operations(
            build_restore_operations(),
            check=check,
            success_message="Reader assets already match restored light-source state",
            unchanged_message=(
                "No changes needed; reader assets already match restored light-source state"
            ),
        )
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 4


def sync_reader_assets(theme_name: str, foreground: str, check: bool = False) -> int:
    source_css = nov_source_path(theme_name)
    try:
        ensure_paths_exist([source_css, PDFJS_VIEWER_HTML, PDFJS_VIEWER_CSS])
    except FileNotFoundError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 3
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 4

    try:
        return run_operations(
            build_sync_operations(theme_name, foreground),
            check=check,
            success_message=(
                f"Reader assets already match {theme_name} theme with transparent background "
                f"and foreground {foreground}"
            ),
            unchanged_message=(
                f"No changes needed; reader assets already match {theme_name} theme "
                f"with transparent background and foreground {foreground}"
            ),
        )
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 4


def main() -> int:
    args = parse_args()

    if args.restore_stock_light_assets:
        return restore_stock_light_reader_assets(check=args.check)

    try:
        foreground = normalize_hex_color(args.foreground)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    return sync_reader_assets(args.theme, foreground, check=args.check)


if __name__ == "__main__":
    sys.exit(main())
