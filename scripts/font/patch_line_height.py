import argparse
import os
import shutil

import fontforge

SFNT_LANGUAGE = "English (US)"
METRIC_PAIRS = (
    ("os2_winascent", "os2_windescent"),
    ("os2_typoascent", "os2_typodescent"),
    ("hhea_ascent", "hhea_descent"),
)


def parse_arguments() -> argparse.Namespace:
    """Parse and return command line arguments."""
    parser = argparse.ArgumentParser(
        description="Patch font line height and normalize naming/style metadata.",
    )
    parser.add_argument("-i", "--input", required=True, help="The original font file")
    parser.add_argument(
        "-o",
        "--outputDir",
        "--output-dir",
        dest="output_dir",
        required=True,
        help="The path to the output directory",
    )
    parser.add_argument(
        "-f",
        "--factor",
        type=float,
        required=True,
        help="The factor by which to multiply the line height",
    )
    parser.add_argument("--style", default="Regular", help="The font style name")
    parser.add_argument("--family", help="The font family name")
    return parser.parse_args()


def adjust_font_properties(font: fontforge.font, factor: float) -> None:
    """Adjust ascent and descent properties of the font."""
    deltas = []
    for ascent_prop, descent_prop in METRIC_PAIRS:
        ascent_value = getattr(font, ascent_prop)
        descent_value = getattr(font, descent_prop)
        delta = int((ascent_value - descent_value) * (1.0 - factor))
        deltas.append(delta)

    for (ascent_prop, descent_prop), delta in zip(METRIC_PAIRS, deltas):
        setattr(font, descent_prop, getattr(font, descent_prop) + delta)
        setattr(font, ascent_prop, getattr(font, ascent_prop) - delta)


def get_output_filename(input_path: str, family: str | None, factor: float) -> str:
    filename, extension = os.path.splitext(os.path.basename(input_path))
    family_part = f"_{family}" if family else ""
    return f"{filename}{family_part}_Patched_{factor}{extension}"


def generate_patched_font(
    font: fontforge.font, output_dir: str, output_filename: str
) -> str:
    """Generate and save the patched font file."""
    os.makedirs(output_dir, exist_ok=True)

    output_path = os.path.join(output_dir, output_filename)
    if os.path.exists(output_path):
        os.unlink(output_path)
    font.generate(output_path)
    print(f"Saved patched font file: {output_filename}")
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


def append_sfnt_name(font: fontforge.font, name_id: str, value: str) -> None:
    font.appendSFNTName(SFNT_LANGUAGE, name_id, value)


def apply_font_metadata(
    font: fontforge.font, family: str, style: str, factor: float
) -> None:
    """Normalize family/style metadata so apps resolve Regular/Italic correctly."""
    style = (style or "Regular").strip()
    full_name = f"{family} {style}".strip()
    patch_tag = str(factor).replace(".", "_")
    postscript_name = f"{family.replace(' ', '')}-{style.replace(' ', '')}-P{patch_tag}"

    font.fontname = postscript_name
    font.familyname = family
    font.fullname = full_name
    append_sfnt_name(font, "Family", family)
    append_sfnt_name(font, "SubFamily", style)
    append_sfnt_name(font, "Fullname", full_name)
    append_sfnt_name(font, "PostScriptName", postscript_name)
    append_sfnt_name(font, "Preferred Family", family)
    append_sfnt_name(font, "Preferred Styles", style)

    style_lower = style.lower()
    is_italic = ("italic" in style_lower) or ("oblique" in style_lower)

    # Normalize weight to Regular so patched Light sources are exposed as
    # Regular/Italic in apps like Emacs/macOS.
    font.weight = "Regular"
    font.os2_weight = 400

    # Update style bits.
    # Some fonts expose macstyle as -1 (unknown in fontforge). Always normalize
    # to valid style bits before writing.
    current_stylemap = int(font.os2_stylemap or 0)
    has_bold = current_stylemap in (2, 3)
    font.macstyle = (1 if has_bold else 0) | (2 if is_italic else 0)
    font.os2_stylemap = (2 if has_bold else 0) | (1 if is_italic else 0)

    version = f"Version 1.0; util-font-patcher {patch_tag}"
    unique_id = f"util-font-patcher;{family};{style};{patch_tag}"
    font.version = version
    append_sfnt_name(font, "Version", version)
    append_sfnt_name(font, "UniqueID", unique_id)


def main() -> None:
    """Main function to orchestrate the font patching process."""
    args = parse_arguments()
    font = fontforge.open(args.input)
    try:
        if args.family:
            apply_font_metadata(font, args.family, args.style, args.factor)
        adjust_font_properties(font, args.factor)
        output_filename = get_output_filename(args.input, args.family, args.factor)
        generate_patched_font(font, args.output_dir, output_filename)
        backup_original_font(args.input, args.output_dir)
    finally:
        font.close()


if __name__ == "__main__":
    main()
