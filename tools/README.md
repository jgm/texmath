# StarMath review tools

`make-starmath-review.py` builds a manual review package for the StarMath writer golden tests. It compares the StarMath output in `test/writer/starmath` with reference TeX renderings, then writes Markdown, ODT, PDF, images, and summary files to an output directory. For each fixture, the package shows the stored fixture StarMath plus regenerated display-mode and inline-mode StarMath translations, with separate LaTeX reference renders for display and inline math contexts.

Run it from anywhere inside a checkout:

```sh
tools/make-starmath-review.py --output-dir /tmp/starmath-review
```

To use a locally patched Pandoc build, pass its executable path:

```sh
tools/make-starmath-review.py --output-dir /tmp/starmath-review --pandoc /path/to/pandoc
```

The script locates the repository root from its own path, so it does not depend on the checkout being in a particular directory. It expects these commands to be available on `PATH`:

- `cabal` and `runghc`, used to regenerate TeX from native fixtures and StarMath display/inline variants; this matches the project CI setup
- `pdflatex` and `pdftocairo`, used to render reference images
- `pandoc`, used to create the ODT review document, unless supplied with `--pandoc`
- `soffice`, used to export the review document as PDF

The review ordering and comments come from `test/writer/starmath-review-status.tsv`. The main outputs are `starmath-review.md`, `starmath-review.odt`, `starmath-review.pdf`, `summary.txt`, image files for the display/inline LaTeX references, and any render-failure logs in the selected output directory.
