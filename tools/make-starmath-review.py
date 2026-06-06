#!/usr/bin/env python3
from __future__ import annotations

import argparse
import html
import re
import shutil
import subprocess
import textwrap
import zipfile
from xml.sax.saxutils import escape
from pathlib import Path
from typing import Optional

ROOT = Path(__file__).resolve().parents[1]
READER_TEX = ROOT / 'test' / 'reader' / 'tex'
WRITER_TEX = ROOT / 'test' / 'writer' / 'tex'
WRITER_STARMATH = ROOT / 'test' / 'writer' / 'starmath'
REVIEW_STATUS_TSV = ROOT / 'test' / 'writer' / 'starmath-review-status.tsv'
REGENERATED_TEX_HELPER = ROOT / 'tools' / 'dump-native-tex.hs'
STARMATH_VARIANTS_HELPER = ROOT / 'tools' / 'dump-starmath-variants.hs'


def parse_test(path: Path):
    s = path.read_text(encoding='utf-8')
    m = re.search(r'^<<<\s*([\w-]+)\n(.*?)^>>>\s*([\w-]+)\n(.*)\Z', s, re.S | re.M)
    if not m:
        raise ValueError(f'Could not parse {path}')
    return m.group(1), m.group(2).strip(), m.group(3), m.group(4).strip()


def shell(cmd, cwd: Optional[Path] = None):
    try:
        return subprocess.run(cmd, cwd=str(cwd) if cwd else None, text=True,
                              capture_output=True, check=False)
    except FileNotFoundError as e:
        raise SystemExit(f"Required command not found: {cmd[0]}") from e


DISPLAY_MATH_ENVS = {
    'align', 'align*', 'equation', 'equation*', 'gather', 'gather*',
    'multline', 'multline*', 'flalign', 'flalign*'
}

MATH_SUB_ENVS = {
    'array', 'matrix', 'pmatrix', 'bmatrix', 'Bmatrix', 'vmatrix',
    'Vmatrix', 'smallmatrix', 'cases', 'aligned', 'alignedat',
    'gathered', 'split'
}

PREAMBLE_COMMAND_PREFIXES = (
    '\\newcommand', '\\renewcommand', '\\newenvironment',
    '\\renewenvironment', '\\DeclareMathOperator'
)


def extract_environment_name(line: str) -> Optional[str]:
    m = re.match(r'\s*\\begin\{([^}]+)\}', line)
    return m.group(1) if m else None


def wrap_display_math(src: str) -> str:
    return '\\[\n' + src.strip() + '\n\\]'


def wrap_inline_math(src: str) -> str:
    return '\\(' + src.strip().replace('\n', ' ') + '\\)'


def starts_preamble_command(line: str) -> bool:
    stripped = line.strip()
    return any(stripped.startswith(prefix) for prefix in PREAMBLE_COMMAND_PREFIXES)


def strip_tex_comment(line: str) -> str:
    out = []
    escaped = False
    for ch in line:
        if ch == '%' and not escaped:
            break
        out.append(ch)
        escaped = (ch == '\\' and not escaped)
        if ch != '\\':
            escaped = False
    return ''.join(out)


def brace_delta(line: str) -> int:
    raw = strip_tex_comment(line)
    return raw.count('{') - raw.count('}')


def split_preamble_and_body(tex_source: str):
    preamble_lines = []
    body_lines = []
    lines = tex_source.splitlines()
    i = 0
    while i < len(lines):
        line = lines[i]
        if starts_preamble_command(line):
            block = [line]
            balance = brace_delta(line)
            i += 1
            while i < len(lines):
                next_line = lines[i]
                next_stripped = next_line.strip()
                if balance <= 0 and not next_stripped.startswith('{'):
                    break
                block.append(next_line)
                balance += brace_delta(next_line)
                i += 1
            preamble_lines.extend(block)
        else:
            body_lines.append(line)
            i += 1
    return '\n'.join(preamble_lines).strip(), body_lines


def build_reference_parts(tex_source: str, display_mode: str):
    stripped_source = tex_source.strip()
    if not stripped_source:
        return '', ''
    preamble_tex, body_lines = split_preamble_and_body(tex_source)
    simple_math = (
        not preamble_tex
        and '\\begin{' not in stripped_source
        and '\\end{' not in stripped_source
    )
    if simple_math:
        body = wrap_display_math(stripped_source) if display_mode == 'display' else wrap_inline_math(stripped_source)
    else:
        chunks: list[str] = []
        i = 0
        while i < len(body_lines):
            line = body_lines[i]
            stripped = line.strip()
            if not stripped:
                i += 1
                continue
            if stripped.startswith('%'):
                chunks.append(line)
                i += 1
                continue
            env = extract_environment_name(line)
            if env is not None:
                block_lines = [line]
                i += 1
                end_pat = f'\\end{{{env}}}'
                while i < len(body_lines):
                    block_lines.append(body_lines[i])
                    if end_pat in body_lines[i]:
                        i += 1
                        break
                    i += 1
                block = '\n'.join(block_lines).strip()
                if env in DISPLAY_MATH_ENVS:
                    chunks.append(block)
                elif env in MATH_SUB_ENVS:
                    chunks.append(wrap_display_math(block) if display_mode == 'display' else wrap_inline_math(block))
                else:
                    chunks.append(wrap_display_math(block) if display_mode == 'display' else wrap_inline_math(block))
                continue

            expr_lines = [line]
            i += 1
            while i < len(body_lines):
                next_line = body_lines[i]
                next_stripped = next_line.strip()
                if not next_stripped or next_stripped.startswith('%') or extract_environment_name(next_line) is not None:
                    break
                expr_lines.append(next_line)
                i += 1
            expr = '\n'.join(expr_lines).strip()
            chunks.append(wrap_display_math(expr) if display_mode == 'display' else wrap_inline_math(expr))
        body = '\n\n'.join(c for c in chunks if c.strip())

    return preamble_tex, body


def render_reference_image(stem: str, tex_source: str, display_mode: str, out_png: Path, build_dir: Path) -> Optional[str]:
    case_dir = build_dir / stem
    case_dir.mkdir(parents=True, exist_ok=True)
    tex_file = case_dir / f'{stem}.tex'
    pdf_file = case_dir / f'{stem}.pdf'
    preamble_tex, body = build_reference_parts(tex_source, display_mode)
    tex_doc = textwrap.dedent(f"""\
    \\documentclass[border=4pt,varwidth]{{standalone}}
    \\usepackage{{amsmath,amssymb,amsfonts,mathtools,cancel}}
    {preamble_tex}
    \\begin{{document}}
    {body}
    \\end{{document}}
    """)
    tex_file.write_text(tex_doc, encoding='utf-8')

    r1 = shell(['pdflatex', '-interaction=nonstopmode', '-halt-on-error', tex_file.name], cwd=case_dir)
    if r1.returncode != 0 or not pdf_file.exists():
        return (r1.stdout + '\n' + r1.stderr).strip()

    out_png.parent.mkdir(parents=True, exist_ok=True)
    out_base = out_png.with_suffix('')
    r2 = shell(['pdftocairo', '-png', '-singlefile', pdf_file.name, out_base.name], cwd=case_dir)
    if r2.returncode != 0:
        return (r2.stdout + '\n' + r2.stderr).strip()

    generated = case_dir / (out_base.name + '.png')
    if not generated.exists():
        return 'pdftocairo did not produce a PNG file'

    shutil.copyfile(generated, out_png)
    return None


def fenced(lang: str, body: str) -> str:
    return f"```{lang}\n{body.rstrip()}\n```\n"


def center_images_in_odt(odt_path: Path) -> None:
    with zipfile.ZipFile(odt_path, 'r') as zin:
        files = {name: zin.read(name) for name in zin.namelist()}

    content = files['content.xml'].decode('utf-8')
    content = re.sub(
        r'<draw:frame draw:name="(img\d+)" svg:width=',
        r'<draw:frame draw:style-name="fr2" text:anchor-type="paragraph" draw:name="\1" svg:width=',
        content,
    )
    content = re.sub(
        r'(<text:p text:style-name=")([^"]+)(">)Review comment:',
        r'\1ReviewComment\3Review comment:',
        content,
    )
    files['content.xml'] = content.encode('utf-8')

    styles = files['styles.xml'].decode('utf-8')
    review_style = (
        '<style:style style:name="ReviewComment" style:family="paragraph" '
        'style:parent-style-name="Text_20_body">'
        '<style:text-properties fo:color="#0066cc"/></style:style>'
    )
    if 'style:name="ReviewComment"' not in styles:
        styles = styles.replace('</office:styles>', review_style + '</office:styles>')
    files['styles.xml'] = styles.encode('utf-8')

    tmp_path = odt_path.with_suffix('.tmp.odt')
    with zipfile.ZipFile(tmp_path, 'w') as zout:
        for name, data in files.items():
            info = zipfile.ZipInfo(name)
            if name == 'mimetype':
                info.compress_type = zipfile.ZIP_STORED
            else:
                info.compress_type = zipfile.ZIP_DEFLATED
            zout.writestr(info, data)
    tmp_path.replace(odt_path)


def formula_annotation_text(annotation: dict) -> str:
    commented_tex = '\n'.join('%% ' + line for line in annotation['tex'].splitlines())
    return (
        annotation['starmath']
        + '\n%% TeX '
        + annotation['mode']
        + ':\n'
        + commented_tex
    )


def mathml_formula_text(annotation: dict) -> str:
    mathml = annotation['mathml'].strip()
    if mathml.startswith('<?xml'):
        return mathml + '\n'
    return "<?xml version='1.0' ?>\n" + mathml + '\n'


def rewrite_formula_object_from_starmath(files: dict[str, bytes], formula_path: str, annotation: dict) -> None:
    formula_xml = files[formula_path].decode('utf-8')
    annotation_text = escape(formula_annotation_text(annotation))

    def repl(m):
        return m.group(1) + annotation_text + m.group(3)

    formula_xml_new, n = re.subn(
        r'(<annotation encoding="StarMath 5\.0">)(.*?)(</annotation>)',
        repl,
        formula_xml,
        flags=re.S,
    )
    if n != 1:
        raise SystemExit(f'Could not rewrite StarMath annotation in {formula_path}')
    files[formula_path] = formula_xml_new.encode('utf-8')


def rewrite_formula_object_from_mathml(files: dict[str, bytes], formula_path: str, annotation: dict) -> None:
    files[formula_path] = mathml_formula_text(annotation).encode('utf-8')


def rewrite_formula_objects(odt_path: Path, annotations: list[dict]) -> None:
    with zipfile.ZipFile(odt_path, 'r') as zin:
        files = {name: zin.read(name) for name in zin.namelist()}

    content = files['content.xml'].decode('utf-8')
    formula_refs = re.findall(r'xlink:href="(Formula-\d+/)"', content)
    if len(formula_refs) != len(annotations):
        raise SystemExit(
            f'Expected {len(annotations)} formula objects in {odt_path}, found {len(formula_refs)}'
        )

    for ref, annotation in zip(formula_refs, annotations):
        formula_path = ref + 'content.xml'
        if formula_path not in files:
            raise SystemExit(f'Missing formula object content: {formula_path}')
        if annotation['kind'] == 'starmath':
            rewrite_formula_object_from_starmath(files, formula_path, annotation)
        elif annotation['kind'] == 'mathml':
            rewrite_formula_object_from_mathml(files, formula_path, annotation)
        else:
            raise SystemExit(f'Unknown formula annotation kind: {annotation["kind"]}')

    tmp_path = odt_path.with_suffix('.tmp.odt')
    with zipfile.ZipFile(tmp_path, 'w') as zout:
        for name, data in files.items():
            info = zipfile.ZipInfo(name)
            if name == 'mimetype':
                info.compress_type = zipfile.ZIP_STORED
            else:
                info.compress_type = zipfile.ZIP_DEFLATED
            zout.writestr(info, data)
    tmp_path.replace(odt_path)


def regenerate_tex_fixtures(input_dir: Path, output_dir: Path) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    cmd = [
        'cabal', 'v2-exec', '--', 'runghc', '-package=texmath',
        str(REGENERATED_TEX_HELPER), str(input_dir), str(output_dir)
    ]
    r = shell(cmd, cwd=ROOT)
    if r.returncode != 0:
        raise SystemExit('native->tex regeneration failed:\n' + r.stdout + '\n' + r.stderr)


def regenerate_starmath_variants(input_dir: Path, output_dir: Path) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    cmd = [
        'cabal', 'v2-exec', '--', 'runghc', '-package=texmath',
        str(STARMATH_VARIANTS_HELPER), str(input_dir), str(output_dir)
    ]
    r = shell(cmd, cwd=ROOT)
    if r.returncode != 0:
        raise SystemExit('StarMath variant regeneration failed:\n' + r.stdout + '\n' + r.stderr)


def load_starmath_variants(stem: str, output_dir: Path) -> dict:
    return {
        'display': (output_dir / f'{stem}.display.starmath').read_text(encoding='utf-8').strip(),
        'inline': (output_dir / f'{stem}.inline.starmath').read_text(encoding='utf-8').strip(),
    }


def load_mathml_variants(stem: str, output_dir: Path) -> dict:
    return {
        'display': (output_dir / f'{stem}.display.mathml').read_text(encoding='utf-8').strip(),
        'inline': (output_dir / f'{stem}.inline.mathml').read_text(encoding='utf-8').strip(),
    }


def render_reference_variant(
    stem: str,
    mode: str,
    tex_source: str,
    render_source: str,
    fallback_tex: Optional[str],
    fallback_source: Optional[str],
    images_dir: Path,
    build_dir: Path,
) -> dict:
    image_path = images_dir / f'{stem}-latex-{mode}.png'
    active_tex = tex_source
    active_source = render_source
    render_error = render_reference_image(
        f'{stem}-latex-{mode}', active_tex, mode, image_path, build_dir
    )
    if (
        render_error is not None
        and fallback_tex
        and fallback_tex.strip() != active_tex.strip()
    ):
        fallback_error = render_reference_image(
            f'{stem}-latex-{mode}', fallback_tex, mode, image_path, build_dir
        )
        if fallback_error is None:
            active_tex = fallback_tex
            active_source = fallback_source or render_source
            render_error = None
    return {
        'mode': mode,
        'tex': active_tex,
        'source': active_source,
        'image_path': image_path,
        'error': render_error,
    }


def load_review_statuses(path: Path):
    lines = path.read_text(encoding='utf-8', errors='replace').splitlines()
    if not lines:
        raise SystemExit(f'Empty review status file: {path}')
    header = lines[0].split('\t')
    if len(header) < 2 or header[0].strip() != 'test' or header[1].strip() != 'status':
        raise SystemExit(f'Invalid review status file header in {path}')
    statuses = {}
    order = []
    allowed = {'', 'A', 'B', 'C'}
    for line in lines[1:]:
        if not line.strip():
            continue
        parts = line.split('\t')
        if len(parts) < 2:
            raise SystemExit(f'Invalid review status row in {path}: {line!r}')
        stem = canonical_stem(parts[0].strip())
        status = parts[1].strip().upper()
        comment = parts[2].strip() if len(parts) >= 3 else ''
        if status not in allowed:
            raise SystemExit(f'Invalid review status {status!r} for {stem} in {path}')
        if stem in statuses:
            raise SystemExit(f'Duplicate review status entry for {stem} in {path}')
        statuses[stem] = {'status': status, 'comment': comment}
        order.append(stem)
    return statuses, order


def status_bucket(status: str) -> str:
    return status if status else 'unreviewed'


def status_sort_key(status: str) -> int:
    order = {'': 0, 'C': 1, 'B': 2, 'A': 3}
    return order[status]


def canonical_stem(stem: str) -> str:
    if re.fullmatch(r'\d+', stem):
        return str(int(stem))
    return stem


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--output-dir', default='/tmp/starmath-review')
    ap.add_argument('--pandoc', default='pandoc',
                    help='pandoc executable to use for ODT generation')
    args = ap.parse_args()

    out_dir = Path(args.output_dir)
    images_dir = out_dir / 'images'
    build_dir = out_dir / '_build'
    starmath_variants_dir = out_dir / '_starmath_variants'
    out_dir.mkdir(parents=True, exist_ok=True)
    images_dir.mkdir(parents=True, exist_ok=True)
    build_dir.mkdir(parents=True, exist_ok=True)

    regenerate_starmath_variants(WRITER_STARMATH, starmath_variants_dir)

    review_statuses, review_order = load_review_statuses(REVIEW_STATUS_TSV)
    review_order_index = {canonical_stem(stem): idx for idx, stem in enumerate(review_order)}

    cases = []
    writer_tex_native_mismatches = []
    reader_stems = {p.stem for p in READER_TEX.glob('*.test')}
    all_starmath = sorted(WRITER_STARMATH.glob('*.test'))
    for starmath_path in all_starmath:
        stem = starmath_path.stem
        starmath_in_fmt, starmath_in_text, _, starmath = parse_test(starmath_path)

        if stem in reader_stems:
            reader_path = READER_TEX / f'{stem}.test'
            in_fmt, original_tex, out_fmt, native_text = parse_test(reader_path)
            if in_fmt != 'tex' or out_fmt != 'native':
                raise ValueError(f'Unexpected formats in {reader_path}')
            if starmath_in_fmt != 'native' or starmath_in_text.strip() != native_text.strip():
                raise ValueError(f'Native mismatch between {reader_path} and {starmath_path}')
            tex_label = 'Original TeX'
            tex_provenance = 'reader/tex'
            render_tex = original_tex
            render_source = 'reader/tex'
            formula_seed_tex = original_tex
            fallback_render_tex = None
            fallback_render_source = None

            writer_tex_path = WRITER_TEX / f'{stem}.test'
            if writer_tex_path.exists():
                _, native_tex_in, _, writer_tex = parse_test(writer_tex_path)
                if native_tex_in.strip() != native_text.strip():
                    writer_tex_native_mismatches.append(stem)
                formula_seed_tex = writer_tex
                fallback_render_tex = writer_tex
                fallback_render_source = 'writer/tex fallback'
        else:
            if starmath_in_fmt == 'tex':
                original_tex = starmath_in_text
                tex_label = 'Original TeX'
                tex_provenance = 'writer/starmath'
                render_tex = original_tex
                render_source = 'writer/starmath'
                formula_seed_tex = original_tex
                fallback_render_tex = None
                fallback_render_source = None
            elif starmath_in_fmt == 'native':
                regenerated_dir = out_dir / '_regenerated_tex'
                regenerate_tex_fixtures(WRITER_STARMATH, regenerated_dir)
                original_tex = (regenerated_dir / f'{stem}.tex').read_text(encoding='utf-8').strip()
                tex_label = 'Regenerated TeX'
                tex_provenance = 'native -> writeTeX'
                render_tex = original_tex
                render_source = 'regenerated'
                formula_seed_tex = original_tex
                fallback_render_tex = None
                fallback_render_source = None
            else:
                raise ValueError(f'Unexpected input format in {starmath_path}: {starmath_in_fmt}')

        reference_renders = {
            mode: render_reference_variant(
                stem,
                mode,
                render_tex,
                render_source,
                fallback_render_tex,
                fallback_render_source,
                images_dir,
                build_dir,
            )
            for mode in ['display', 'inline']
        }
        starmath_variants = load_starmath_variants(stem, starmath_variants_dir)
        mathml_variants = load_mathml_variants(stem, starmath_variants_dir)

        cases.append({
            'stem': stem,
            'is_reader_case': stem in reader_stems,
            'tex_label': tex_label,
            'tex_provenance': tex_provenance,
            'original_tex': original_tex,
            'render_tex': render_tex,
            'render_source': render_source,
            'reference_renders': reference_renders,
            'formula_seed_tex': formula_seed_tex,
            'fixture_starmath': starmath,
            'starmath_variants': starmath_variants,
            'mathml_variants': mathml_variants,
            'review_status': review_statuses[canonical_stem(stem)]['status'],
            'review_comment': review_statuses[canonical_stem(stem)]['comment'],
        })

    stems_in_cases = {c['stem'] for c in cases}
    stems_in_cases_canonical = {canonical_stem(stem) for stem in stems_in_cases}
    missing_status_entries = sorted(stems_in_cases_canonical - set(review_statuses))
    extra_status_entries = sorted(set(review_statuses) - stems_in_cases_canonical)
    if missing_status_entries:
        raise SystemExit('Missing review status entries for: ' + ', '.join(missing_status_entries))
    if extra_status_entries:
        raise SystemExit('Review status entries with no corresponding case: ' + ', '.join(extra_status_entries))

    cases.sort(key=lambda c: (status_sort_key(c['review_status']), review_order_index[canonical_stem(c['stem'])]))

    md = []
    md.append('% StarMath LibreOffice Review')
    md.append('% Generated from `test/writer/starmath` with review ordering from `test/writer/starmath-review-status.tsv`')
    md.append('% ')
    md.append('')
    md.append('This document is for manual LibreOffice review of the full current StarMath writer corpus.')
    md.append('')
    md.append('Notes')
    md.append('')
    md.append('- `Original TeX` means the source came from `test/reader/tex`.')
    md.append('- `Regenerated TeX` means the TeX was produced from the `native` AST via `writeTeX`.')
    md.append('- For legacy bespoke StarMath regressions, `Original TeX` comes from the `writer/starmath` fixture itself.')
    md.append('- `Fixture StarMath` is the expected writer output stored in `test/writer/starmath`.')
    md.append('- `Display StarMath` and `Inline StarMath` are regenerated from the fixture input using `writeStarMath DisplayBlock` and `writeStarMath DisplayInline`.')
    md.append('- `Display MathML` and `Inline MathML` are regenerated from the fixture input using `writeMathML DisplayBlock` and `writeMathML DisplayInline`.')
    md.append('- `LaTeX reference` images are rendered in both display and inline math contexts, using `writer/tex` output when available, otherwise from the TeX shown in that section.')
    md.append('- `LibreOffice formula` entries are live formula objects whose embedded StarMath source is rewritten from the corresponding display or inline translation.')
    md.append('- `LibreOffice formula (MathML)` entries are live formula objects whose embedded object XML is rewritten to the corresponding MathML without a StarMath annotation.')
    md.append('- Review ordering is: unreviewed first, then `C`, then `B`, then `A`.')
    md.append('- Review status `A`: excellent match.')
    md.append('- Review status `B`: some minor issues.')
    md.append('- Review status `C`: substantially wrong.')
    md.append('')
    failures = []
    formula_annotations = []
    for label in ['unreviewed', 'C', 'B', 'A']:
        heading = {
            'unreviewed': '# Unreviewed',
            'C': '# Review Status C',
            'B': '# Review Status B',
            'A': '# Review Status A',
        }[label]
        md.append(heading)
        md.append('')
        for case in [c for c in cases if status_bucket(c['review_status']) == label]:
            stem = case['stem']
            md.append(f'## {stem}')
            md.append('')
            if case['review_comment']:
                md.append(
                    f'<span style="color: #0066cc;">'
                    f'Review comment: {html.escape(case["review_comment"])}'
                    f'</span>'
                )
                md.append('')
            md.append(case['tex_label'])
            md.append('')
            md.append(fenced('tex', case['original_tex']))
            md.append(f"TeX provenance: `{case['tex_provenance']}`")
            md.append('')
            md.append('Fixture StarMath')
            md.append('')
            md.append(fenced('text', case['fixture_starmath']))
            for mode in ['display', 'inline']:
                title = mode.capitalize()
                render = case['reference_renders'][mode]
                md.append(f'{title} StarMath')
                md.append('')
                md.append(fenced('text', case['starmath_variants'][mode]))
                md.append(f'{title} LaTeX reference render')
                md.append('')
                md.append(f'Reference render source: `{render["source"]}`')
                md.append('')
                if render['source'] not in {'reader/tex', 'writer/starmath'} and \
                   render['tex'].strip() != case['original_tex'].strip():
                    md.append(f'{title} rendered TeX')
                    md.append('')
                    md.append(fenced('tex', render['tex']))
                if render['error'] is None:
                    rel_image = render['image_path'].relative_to(out_dir)
                    md.append(f'![]({rel_image.as_posix()})')
                else:
                    failures.append((stem, mode, render['error']))
                    md.append(f'{title} reference render failed.')
                    md.append('')
                    md.append(fenced('text', render['error']))
                md.append('')
                md.append(f'LibreOffice formula ({mode} StarMath)')
                md.append('')
                md.append('$$')
                md.append(case['formula_seed_tex'])
                md.append('$$')
                md.append('')
                formula_annotations.append({
                    'kind': 'starmath',
                    'mode': mode,
                    'starmath': case['starmath_variants'][mode],
                    'tex': render['tex'],
                })
                md.append(f'{title} MathML')
                md.append('')
                md.append(fenced('xml', case['mathml_variants'][mode]))
                md.append(f'LibreOffice formula ({mode} MathML)')
                md.append('')
                md.append('$$')
                md.append(case['formula_seed_tex'])
                md.append('$$')
                md.append('')
                formula_annotations.append({
                    'kind': 'mathml',
                    'mode': mode,
                    'mathml': case['mathml_variants'][mode],
                    'tex': render['tex'],
                })
    review_md = out_dir / 'starmath-review.md'
    review_md.write_text('\n'.join(md), encoding='utf-8')

    failures_txt = out_dir / 'reference-render-failures.txt'
    if failures:
        failures_txt.write_text('\n\n'.join(f'{stem} ({mode})\n{err}' for stem, mode, err in failures), encoding='utf-8')
    else:
        failures_txt.write_text('', encoding='utf-8')

    review_odt = out_dir / 'starmath-review.odt'
    pandoc_cmd = [
        args.pandoc,
        str(review_md),
        '-f', 'markdown+tex_math_dollars',
        '--resource-path', str(out_dir),
        '--toc',
        '-t', 'odt',
        '-o', str(review_odt),
    ]
    r_pandoc = shell(pandoc_cmd)
    if r_pandoc.returncode != 0:
        raise SystemExit('pandoc ODT generation failed:\n' + r_pandoc.stdout + '\n' + r_pandoc.stderr)

    rewrite_formula_objects(review_odt, formula_annotations)
    center_images_in_odt(review_odt)

    r_pdf = shell(['soffice', '--headless', '--convert-to', 'pdf', '--outdir', str(out_dir), str(review_odt)])
    if r_pdf.returncode != 0:
        raise SystemExit('LibreOffice PDF export failed:\n' + r_pdf.stdout + '\n' + r_pdf.stderr)

    summary = textwrap.dedent(f'''\
    Cases: {len(cases)}
    Reader/tex cases: {sum(1 for c in cases if c['is_reader_case'])}
    Additional StarMath-only cases: {sum(1 for c in cases if not c['is_reader_case'])}
    Unreviewed cases: {sum(1 for c in cases if c['review_status'] == '')}
    Review status C cases: {sum(1 for c in cases if c['review_status'] == 'C')}
    Review status B cases: {sum(1 for c in cases if c['review_status'] == 'B')}
    Review status A cases: {sum(1 for c in cases if c['review_status'] == 'A')}
    Reference render failures: {len(failures)}
    writer/tex native mismatches by stem: {len(writer_tex_native_mismatches)}
    Review status file: {REVIEW_STATUS_TSV}
    Markdown: {review_md}
    ODT: {review_odt}
    PDF: {out_dir / 'starmath-review.pdf'}
    Failure log: {failures_txt}
    ''')
    (out_dir / 'summary.txt').write_text(summary, encoding='utf-8')
    if writer_tex_native_mismatches:
        (out_dir / 'writer-tex-native-mismatches.txt').write_text(
            '\n'.join(writer_tex_native_mismatches) + '\n', encoding='utf-8'
        )
    print(summary)


if __name__ == '__main__':
    main()
