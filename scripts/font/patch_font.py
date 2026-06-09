import argparse
import os
import shutil
import unicodedata

import fontforge

SFNT_LANGUAGE = "English (US)"

METRIC_PAIRS = (
    ("os2_winascent", "os2_windescent"),
    ("os2_typoascent", "os2_typodescent"),
    ("hhea_ascent", "hhea_descent"),
)

TOOL_NAME = "font-patcher"


def is_cjk(unicode_val: int) -> bool:
    try:
        return unicodedata.east_asian_width(chr(unicode_val)) in ("W", "F")
    except (ValueError, OverflowError):
        return False


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Adjust a font's line height and/or align its glyph widths in a single pass.",
    )
    parser.add_argument("-i", "--input", required=True, help="The font file to patch")
    parser.add_argument(
        "-o",
        "--outputDir",
        "--output-dir",
        dest="output_dir",
        required=True,
        help="The path to the output directory",
    )
    parser.add_argument(
        "--width-ref",
        dest="width_ref",
        default=None,
        help="Reference font to match width metrics against (enables width matching)",
    )
    parser.add_argument(
        "--width-scale",
        dest="width_scale",
        type=float,
        default=None,
        help="Optional glyph-outline scale applied during width matching "
        "(only meaningful with --width-ref)",
    )
    parser.add_argument(
        "--line-height",
        dest="line_height",
        type=float,
        default=None,
        help="Line-height factor; scales the ascent/descent span (enables line-height adjustment)",
    )
    parser.add_argument("--family", default=None, help="The new font family name")
    parser.add_argument("--style", default="Regular", help="The font style name")

    args = parser.parse_args()
    if args.width_ref is None and args.line_height is None:
        parser.error("nothing to do; pass --width-ref and/or --line-height")
    return args


def append_sfnt_name(font: fontforge.font, name_id: str, value: str) -> None:
    font.appendSFNTName(SFNT_LANGUAGE, name_id, value)


def apply_font_metadata(font: fontforge.font, family: str, style: str, tag: str) -> None:
    """Normalize family/style metadata so apps resolve Regular/Italic correctly."""
    style = (style or "Regular").strip()
    full_name = f"{family} {style}".strip()
    postscript_name = f"{family.replace(' ', '')}-{style.replace(' ', '')}-{tag}"

    font.fontname = postscript_name
    font.familyname = family
    font.fullname = full_name
    for name_id, value in [
        ("Family", family),
        ("SubFamily", style),
        ("Fullname", full_name),
        ("PostScriptName", postscript_name),
        ("Preferred Family", family),
        ("Preferred Styles", style),
    ]:
        append_sfnt_name(font, name_id, value)

    font.weight = "Regular"
    font.os2_weight = 400

    style_lower = style.lower()
    is_italic = ("italic" in style_lower) or ("oblique" in style_lower)

    # Some fonts expose macstyle as -1 (unknown in fontforge). Always normalize
    # to valid style bits, preserving any existing bold bit.
    current_stylemap = int(font.os2_stylemap or 0)
    has_bold = current_stylemap in (2, 3)
    font.macstyle = (1 if has_bold else 0) | (2 if is_italic else 0)
    font.os2_stylemap = (2 if has_bold else 0) | (1 if is_italic else 0)

    version = f"Version 1.0; {TOOL_NAME} {tag}"
    unique_id = f"{TOOL_NAME};{family};{style};{tag}"
    font.version = version
    append_sfnt_name(font, "Version", version)
    append_sfnt_name(font, "UniqueID", unique_id)


def fix_glyph_widths(input_font: fontforge.font, ref_font: fontforge.font) -> tuple[int, int]:
    ref_width = ref_font["m"].width if "m" in ref_font else ref_font["A"].width
    single_width = round(ref_width * input_font.em / ref_font.em)
    double_width = single_width * 2
    cjk_count = 0
    half_count = 0

    for glyph in input_font.glyphs():
        if glyph.unicode < 0:
            continue
        # Preserve zero-advance glyphs (combining marks, control chars, etc.).
        if glyph.width == 0:
            continue
        if is_cjk(glyph.unicode):
            glyph.width = double_width
            cjk_count += 1
        else:
            glyph.width = single_width
            half_count += 1

    return cjk_count, half_count


def scale_glyphs(font: fontforge.font, scale: float) -> None:
    for glyph in font.glyphs():
        old_width = glyph.width
        if len(glyph.layers) > 1:
            glyph.unlinkRef()
        glyph.transform((scale, 0, 0, scale, 0, 0))
        glyph.width = old_width
        glyph.round()


def adjust_font_properties(font: fontforge.font, factor: float) -> None:
    """Adjust ascent and descent properties of the font by a line-height factor."""
    deltas = []
    for ascent_prop, descent_prop in METRIC_PAIRS:
        ascent_value = getattr(font, ascent_prop)
        descent_value = getattr(font, descent_prop)
        delta = int((ascent_value - descent_value) * (1.0 - factor))
        deltas.append(delta)

    for (ascent_prop, descent_prop), delta in zip(METRIC_PAIRS, deltas):
        setattr(font, descent_prop, getattr(font, descent_prop) + delta)
        setattr(font, ascent_prop, getattr(font, ascent_prop) - delta)


def generate_patched_font(
    font: fontforge.font, output_dir: str, output_filename: str
) -> str:
    """Generate and save the patched font file."""
    os.makedirs(output_dir, exist_ok=True)

    output_path = os.path.join(output_dir, output_filename)
    if os.path.exists(output_path):
        os.unlink(output_path)
    font.generate(output_path)
    print(f"Saved patched font: {output_path}")
    return output_path


def backup_original_font(input_path: str, output_dir: str) -> None:
    """Copy the original font to the output/backup folder."""
    backup_dir = os.path.join(output_dir, "backup")
    os.makedirs(backup_dir, exist_ok=True)
    original_filename = os.path.basename(input_path)
    backup_path = os.path.join(backup_dir, original_filename)

    if os.path.realpath(input_path) != os.path.realpath(backup_path):
        shutil.copy2(input_path, backup_path)
        print(f"Copied original font to: {backup_path}")


def main() -> None:
    args = parse_arguments()
    do_width = args.width_ref is not None
    do_line_height = args.line_height is not None

    ref_font = fontforge.open(args.width_ref) if do_width else None
    font = fontforge.open(args.input)
    try:
        cjk_count = half_count = 0
        width_tag = line_height_tag = None

        # Line-height runs first: it owns the vertical metrics (ascent/descent).
        if do_line_height:
            adjust_font_properties(font, args.line_height)
            line_height_tag = f"L{str(args.line_height).replace('.', '_')}"

        # Width runs last and only aligns glyph advance widths (and optionally scales
        # outlines). It never touches vertical metrics, so the line-height above is
        # preserved; running it last keeps the aligned widths authoritative.
        if do_width:
            cjk_count, half_count = fix_glyph_widths(font, ref_font)
            if args.width_scale:
                scale_glyphs(font, args.width_scale)
            scale_tag = str(args.width_scale).replace(".", "_") if args.width_scale else "NS"
            width_tag = f"M{scale_tag}"

        tag = "-".join(t for t in (width_tag, line_height_tag) if t)

        if args.family:
            apply_font_metadata(font, args.family, args.style, tag)

        extension = os.path.splitext(args.input)[1]
        if args.family:
            base = f"{args.family.replace(' ', '')}-{args.style.replace(' ', '')}"
        else:
            base = os.path.splitext(os.path.basename(args.input))[0]
        output_filename = f"{base}-{tag}{extension}"

        generate_patched_font(font, args.output_dir, output_filename)
        backup_original_font(args.input, args.output_dir)

        if do_width:
            print(f"Normalized {half_count} half-width glyphs and {cjk_count} CJK glyphs (2x)")
        if do_line_height:
            print(f"Applied line-height factor {args.line_height}")
    finally:
        font.close()
        if ref_font is not None:
            ref_font.close()


if __name__ == "__main__":
    main()
