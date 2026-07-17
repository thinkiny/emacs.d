import argparse
import os
import shutil
import statistics
import unicodedata

import fontforge

SFNT_LANGUAGE = "English (US)"

METRIC_PAIRS = (
    ("os2_winascent", "os2_windescent"),
    ("os2_typoascent", "os2_typodescent"),
    ("hhea_ascent", "hhea_descent"),
)

TOOL_NAME = "font-patcher"


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
    parser.add_argument(
        "--copy-symbol",
        dest="copy_symbol",
        default=None,
        help="Path to a donor font; ASCII, symbol, and PUA-icon glyphs "
        "missing from the input are copied in (CJK excluded), so they "
        "render at the correct cell width instead of falling back to "
        "another font.",
    )
    parser.add_argument("--family", default=None, help="The new font family name")
    parser.add_argument("--style", default="Regular", help="The font style name")
    return parser.parse_args()


def _append_sfnt_name(font: fontforge.font, name_id: str, value: str) -> None:
    font.appendSFNTName(SFNT_LANGUAGE, name_id, value)


def apply_font_metadata(
    font: fontforge.font, family: str, style: str, tag: str
) -> None:
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
        _append_sfnt_name(font, name_id, value)

    font.weight = "Regular"
    font.os2_weight = 400

    style_lower = style.lower()
    is_italic = ("italic" in style_lower) or ("oblique" in style_lower)

    current_stylemap = int(font.os2_stylemap or 0)
    has_bold = current_stylemap in (2, 3)
    font.macstyle = (1 if has_bold else 0) | (2 if is_italic else 0)
    font.os2_stylemap = (2 if has_bold else 0) | (1 if is_italic else 0)

    version = f"Version 1.0; {TOOL_NAME} {tag}"
    unique_id = f"{TOOL_NAME};{family};{style};{tag}"
    font.version = version
    _append_sfnt_name(font, "Version", version)
    _append_sfnt_name(font, "UniqueID", unique_id)


def align_glyph_widths(
    input_font: fontforge.font, ref_widths: dict[int, int]
) -> tuple[int, int]:
    ref_glyph_width = ref_widths.get(ord("m"), ref_widths.get(ord("A")))
    double_width = ref_glyph_width * 2

    cjk_count = 0
    half_count = 0

    for glyph in input_font.glyphs():
        if glyph.unicode < 0 or glyph.width == 0:
            continue

        try:
            ea_width = unicodedata.east_asian_width(chr(glyph.unicode))
        except (ValueError, OverflowError):
            ea_width = "N"

        if ea_width in ("W", "F"):
            new_width = double_width
            cjk_count += 1
        else:
            # Non-CJK glyphs are one cell in a monospace target. Don't borrow the
            # CJK ref's per-glyph width -- it inflates neutral symbols (U+25CA,
            # U+FB01..FB04) to full-width.
            new_width = ref_glyph_width
            half_count += 1

        # Recenter so width change splits evenly across both sidebearings
        delta = new_width - glyph.width
        if delta:
            glyph.transform((1, 0, 0, 1, delta / 2, 0))
            glyph.width = new_width

    return cjk_count, half_count


def copy_missing_glyphs(
    input_font: fontforge.font,
    donor_path: str,
    single_width: int | None = None,
) -> int:
    """Copy missing glyphs from donor, scaling each uniformly into the target cell.

    One scale (target cell width / donor glyph width) is applied to both axes,
    so the donor glyph keeps its aspect ratio and simply renders at the target
    cell size.  Sidebearings are then recentered so positioning matches the
    glyphs normalized by align_glyph_widths.
    """
    existing = {g.unicode for g in input_font.glyphs() if g.unicode >= 0}

    input_font.mergeFonts(donor_path)
    added = 0
    for glyph in input_font.glyphs():
        cp = glyph.unicode
        if cp < 0 or cp in existing:
            continue

        # One uniform scale maps the donor glyph's cell design into the target
        # cell, preserving the glyph's aspect ratio.  The ratio is taken from
        # the merged glyph width (already in input-em units after mergeFonts
        # adopts the input em), so it accounts for any em difference between
        # donor and input -- a separate em-ratio transform would double-apply
        # that correction and break the X/Y proportionality.
        if single_width and glyph.width > 0 and glyph.width != single_width:
            scale = single_width / glyph.width
            glyph.transform((scale, 0, 0, scale, 0, 0))

        # Recenter so the residual width change splits evenly across both
        # sidebearings, matching how align_glyph_widths treats existing
        # glyphs (consistent positioning across the font).
        delta = (single_width - glyph.width) if single_width else 0
        if delta:
            glyph.transform((1, 0, 0, 1, delta / 2, 0))
        if single_width:
            glyph.width = single_width
        added += 1
    return added


def scale_glyph_outlines(font: fontforge.font, scale: float) -> None:
    for glyph in font.glyphs():
        old_width = glyph.width
        if len(glyph.layers) > 1:
            glyph.unlinkRef()
        glyph.transform((scale, 0, 0, scale, 0, 0))
        glyph.width = old_width
        glyph.round()


def compute_ink_envelope(font: fontforge.font) -> tuple[float, float]:
    """Median ink extents of mapped glyphs, in font units.

    Returns (median_top, median_bottom): the median of each glyph's
    highest ink, and the median of its below-baseline ink.  The median
    ignores the oversized tail (accents, ``|``, box drawing, PUA icons
    spanning the em) so a few icons can't inflate the line height and
    push ordinary text to the cell bottom.  The bottom uses only
    below-baseline values so it tracks real descender depth rather than
    the baseline most glyphs sit on.
    """
    tops: list[float] = []
    descenders: list[float] = []
    for glyph in font.glyphs():
        if glyph.unicode < 0:
            continue
        try:
            _, min_y, _, max_y = glyph.boundingBox()
        except (TypeError, ValueError):
            continue
        if max_y == 0 and min_y == 0:
            continue
        tops.append(max_y)
        if min_y < 0:
            descenders.append(min_y)

    if not tops:
        return font.os2_typoascent, font.os2_typodescent

    return statistics.median(tops), (
        statistics.median(descenders) if descenders else 0.0
    )


def scale_line_height(font: fontforge.font, factor: float) -> None:
    """Scale the line height by `factor` and re-center it on the ink envelope.

    The total ascent+descent span is scaled by `factor`, then ascent/descent
    are set so the median glyph ink range sits centered in the new line --
    equal padding above the median top and below the median below-baseline
    bottom.  Centering on the medians (not the raw min/max) keeps oversized
    glyphs in the tails from dominating, so ordinary text stays optically
    centered instead of drooping to the cell bottom.

    Typo line gap is folded into the typo ascent first so all three metric pairs
    describe the same total line height after scaling.
    """
    # Fold the typolinegap into typoascent so all pairs share the same
    # effective span.  Without this, Emacs adds typolinegap on top of
    # the typo span, breaking alignment with win/hhea metrics.
    line_gap = font.os2_typolinegap
    if line_gap:
        font.os2_typoascent += line_gap
        font.os2_typolinegap = 0

    ink_top, ink_bottom = compute_ink_envelope(font)
    ink_height = ink_top - ink_bottom

    for ascent_prop, descent_prop in METRIC_PAIRS:
        ascent_value = getattr(font, ascent_prop)
        descent_value = getattr(font, descent_prop)
        # Win descent is unsigned; typo/hhea descent is signed
        is_win = "windescent" in descent_prop
        span = ascent_value + descent_value if is_win else ascent_value - descent_value
        new_span = round(span * factor)

        # Center ink envelope in the scaled line
        pad = (new_span - ink_height) / 2
        new_ascent = round(ink_top + pad)
        descent_mag = new_span - new_ascent
        setattr(font, ascent_prop, new_ascent)
        setattr(font, descent_prop, descent_mag if is_win else -descent_mag)


def generate_output_font(
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


def backup_input_font(input_path: str, output_dir: str) -> None:
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
    symbol_font = fontforge.open(args.copy_symbol) if args.copy_symbol else None
    font = fontforge.open(args.input)

    # Extract ref widths before glyph operations (fontforge may invalidate auxiliary fonts)
    single_width = None
    ref_widths: dict[int, int] = {}
    if ref_font is not None:
        ref_glyph = ref_font["m"] if "m" in ref_font else ref_font["A"]
        single_width = round(ref_glyph.width * font.em / ref_font.em)
        for g in ref_font.glyphs():
            if g.unicode >= 0 and g.width > 0:
                ref_widths[g.unicode] = g.width

    cjk_count = half_count = 0
    width_tag = line_height_tag = None

    if do_line_height:
        scale_line_height(font, args.line_height)
        line_height_tag = f"L{str(args.line_height).replace('.', '_')}"

    # Width alignment before copy_symbol so copied glyphs don't get recentered
    if do_width:
        cjk_count, half_count = align_glyph_widths(font, ref_widths)
        if args.width_scale:
            scale_glyph_outlines(font, args.width_scale)

        scale_tag = (
            str(args.width_scale).replace(".", "_") if args.width_scale else "NS"
        )
        width_tag = f"M{scale_tag}"

    if symbol_font is not None:
        n = copy_missing_glyphs(font, args.copy_symbol, single_width)
        print(f"Copied {n} ASCII/symbol glyph(s) from {args.copy_symbol}")

    tag = "-".join(t for t in (width_tag, line_height_tag) if t)

    if args.family:
        apply_font_metadata(font, args.family, args.style, tag)

    extension = os.path.splitext(args.input)[1]
    if args.family:
        base = f"{args.family.replace(' ', '')}-{args.style.replace(' ', '')}"
    else:
        base = os.path.splitext(os.path.basename(args.input))[0]
    output_filename = f"{base}-{tag}{extension}"

    generate_output_font(font, args.output_dir, output_filename)
    backup_input_font(args.input, args.output_dir)

    if do_width:
        print(
            f"Normalized {half_count} half-width glyphs and {cjk_count} CJK glyphs (2x)"
        )
    if do_line_height:
        print(f"Applied line-height factor {args.line_height}")


if __name__ == "__main__":
    main()
