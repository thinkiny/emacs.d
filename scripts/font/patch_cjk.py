import argparse
import os
import unicodedata

import fontforge

SFNT_LANGUAGE = "English (US)"

METRIC_PAIRS = (
    ("os2_winascent", "os2_windescent"),
    ("os2_typoascent", "os2_typodescent"),
    ("hhea_ascent", "hhea_descent"),
)

METRIC_SINGLES = ("os2_typolinegap", "hhea_linegap")

def is_cjk(unicode_val: int) -> bool:
    try:
        return unicodedata.east_asian_width(chr(unicode_val)) in ("W", "F")
    except (ValueError, OverflowError):
        return False


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Match font metrics to a reference font and fix CJK width.",
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
        "-r",
        "--reference",
        required=True,
        help="Reference font to match metrics against",
    )
    parser.add_argument(
        "-s",
        "--scale",
        type=float,
        default=None,
        help="Scale factor for glyphs (optional). Omit to skip glyph scaling.",
    )
    parser.add_argument("--family", required=True, help="The new font family name")
    parser.add_argument("--style", default="Regular", help="The font style name")
    return parser.parse_args()


def apply_font_metadata(font: fontforge.font, family: str, style: str, tag: str) -> None:
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
        font.appendSFNTName(SFNT_LANGUAGE, name_id, value)

    font.weight = "Regular"
    font.os2_weight = 400

    style_lower = style.lower()
    is_italic = "italic" in style_lower or "oblique" in style_lower
    font.macstyle = 2 if is_italic else 0
    font.os2_stylemap = 1 if is_italic else 0

    version = f"Version 1.0; font-matcher {tag}"
    unique_id = f"font-matcher;{family};{style};{tag}"
    font.version = version
    font.appendSFNTName(SFNT_LANGUAGE, "Version", version)
    font.appendSFNTName(SFNT_LANGUAGE, "UniqueID", unique_id)


def match_metrics(input_font: fontforge.font, ref_font: fontforge.font) -> None:
    em_ratio = input_font.em / ref_font.em

    for ascent_prop, descent_prop in METRIC_PAIRS:
        ref_ascent = getattr(ref_font, ascent_prop)
        ref_descent = getattr(ref_font, descent_prop)
        setattr(input_font, ascent_prop, round(ref_ascent * em_ratio))
        setattr(input_font, descent_prop, round(ref_descent * em_ratio))

    for prop in METRIC_SINGLES:
        ref_val = getattr(ref_font, prop)
        setattr(input_font, prop, round(ref_val * em_ratio))


def fix_cjk_width(input_font: fontforge.font, ref_font: fontforge.font) -> int:
    ref_width = ref_font["m"].width if "m" in ref_font else ref_font["A"].width
    target_cjk_width = round(ref_width * 2 * input_font.em / ref_font.em)
    count = 0

    for glyph in input_font.glyphs():
        if glyph.unicode >= 0 and is_cjk(glyph.unicode):
            glyph.width = target_cjk_width
            count += 1

    return count


def scale_glyphs(font: fontforge.font, scale: float) -> None:
    for glyph in font.glyphs():
        old_width = glyph.width
        if len(glyph.layers) > 1:
            glyph.unlinkRef()
        glyph.transform((scale, 0, 0, scale, 0, 0))
        glyph.width = old_width
        glyph.round()


def main() -> None:
    args = parse_arguments()
    ref_font = fontforge.open(args.reference)
    font = fontforge.open(args.input)
    try:
        match_metrics(font, ref_font)
        cjk_count = fix_cjk_width(font, ref_font)
        if args.scale:
            scale_glyphs(font, args.scale)
        scale_tag = str(args.scale).replace(".", "_") if args.scale else "NS"
        tag = f"M{scale_tag}"
        apply_font_metadata(font, args.family, args.style, tag)
        os.makedirs(args.output_dir, exist_ok=True)
        output_filename = f"{args.family.replace(' ', '')}-{args.style.replace(' ', '')}-{tag}{os.path.splitext(args.input)[1]}"
        output_path = os.path.join(args.output_dir, output_filename)
        if os.path.exists(output_path):
            os.unlink(output_path)
        font.generate(output_path)
        print(f"Adjusted {cjk_count} CJK glyphs to 2x reference width")
        print(f"Saved matched font: {output_path}")
    finally:
        font.close()
        ref_font.close()


if __name__ == "__main__":
    main()
