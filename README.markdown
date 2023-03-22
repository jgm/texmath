# texmath

[![CI
tests](https://github.com/jgm/texmath/workflows/CI%20tests/badge.svg)](https://github.com/jgm/texmath/actions)

texmath is a Haskell library for converting between formats used to
represent mathematics.  Currently it provides functions to read and
write TeX math, presentation MathML, and OMML (Office Math Markup
Language, used in Microsoft Office), and to write Gnu eqn, typst, and
[pandoc]'s native format (allowing conversion, using pandoc, to
a variety of different markup formats).  The TeX reader and
writer supports basic LaTeX and AMS extensions, and it can parse
and apply LaTeX macros.  The package also includes several
utility modules which may be useful for anyone looking to
manipulate either TeX math or MathML.  For example, a copy of
the MathML operator dictionary is included.

[pandoc]: http://github.com/jgm/pandoc

You can [try it out online here](http://johnmacfarlane.net/texmath.html).

By default, only the Haskell library is installed.  To install a
test program, `texmath`, use the `executable` Cabal flag:

    cabal install -fexecutable

By default, the executable will be installed in `~/.cabal/bin`.

Alternatively, texmath can be installed using
[stack](https://github.com/commercialhaskell/stack).  Install
the stack binary somewhere in your path.  Then, in the texmath
repository,

    stack setup
    stack install --flag texmath:executable

The `texmath` binary will be put in `~/.local/bin`.

Macro definitions may be included before a LaTeX formula.

# Running texmath as a server

`texmath` will behave as a CGI script when called under the name
`texmath-cgi` (e.g. through a symbolic link).
The file `cgi/texmath.html` contains an example of how it can
be used.

But it is also possible to compile a full webserver with a JSON
API.  To do this, set the `server` cabal flag, e.g.

    stack install --flag texmath:server

To run the server on port 3000:

    texmath-server -p 3000

Sample of use, with `httpie`:

```
% http --verbose localhost:3000/convert text='2^2' from=tex to=mathml display:=false Accept:'text/plain'
POST /convert HTTP/1.1
Accept: text/plain
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 64
Content-Type: application/json
Host: localhost:3000
User-Agent: HTTPie/3.1.0

{
    "display": false,
    "from": "tex",
    "text": "2^2",
    "to": "mathml"
}


HTTP/1.1 200 OK
Content-Type: text/plain;charset=utf-8
Date: Mon, 21 Mar 2022 18:29:26 GMT
Server: Warp/3.3.17
Transfer-Encoding: chunked

<math display="inline" xmlns="http://www.w3.org/1998/Math/MathML">
  <msup>
    <mn>2</mn>
    <mn>2</mn>
  </msup>
</math>
```

Possible values for `from` are `tex`, `mathml`, and `omml`.
Possible values for `to` are `tex`, `mathml`, `omml`, `eqn`, and
`pandoc` (JSON-encoded Pandoc).

Alternatively, you can use the `convert-batch` endpoint to pass
in a JSON-encoded list of conversions and get back a JSON-encoded
list of results.

# Generating lookup tables

There are three main lookup tables which are built form externally compiled lists.
This section contains information about how to modify and regenerate these tables.

In the `lib` direction there are two sub-directories which contain the
necessary files.

## MMLDict.hs

The utility program `xsltproc` is required.
You can find these files in `lib/mmldict/`

  1. If desired replace `unicode.xml` with and updated version (you can download a copy from [here](http://www.w3.org/TR/xml-entity-names/#source)
  2. `xsltproc -o dictionary.xml operatorDictionary.xsl unicode.xml`
  3. `runghc generateMMLDict.hs`
  4. Replace the operator table at the bottom of `src/Text/TeXMath/Readers/MathML/MMLDict.hs` with the contents of `mmldict.hs`

## ToTeXMath.hs

You can find these files in `lib/totexmath/`

  1. If desired, replace `unimathsymbols.txt` with an updated version from [here](http://milde.users.sourceforge.net/LUCR/Math/)
  2. `runghc unicodetotex.hs`
  3. Replace the record table at the bottom of `src/Text/TeXMath/Unicode/ToTeXMath.hs` with the contents of `UnicodeToLaTeX.hs`

## ToUnicode.hs

You can find these files in `lib/tounicode/`.

  1. If desired, replace `UnicodeData.txt` with an updated verson from
     [here](ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt).
  2. `runghc mkUnicodeTable.hs`
  3. Replace the table at the bottom of
     `src/Text/TeXMath/Unicode/ToUnicode.hs` with the output.

## Editing the tables

It is not necessary to edit the source files to add records to
the tables.  To add to or modify a table it is easier to add
modify either `unicodetotex.hs` or `generateMMLDict.hs`. This is
easily achieved by adding an item to the corresponding `updates`
lists. After making the changes, follow the above steps to
regenerate the table.

# The test suite

To run the test suite, do `cabal test` or `stack test`.

In its standard mode, the test suite will run golden tests of
the individual readers and writers.  Reader tests can be found
in `test/reader/{mml,omml,tex}`, and writer tests in
`test/writer/{eqn,mml,omml,tex}`.  Regression tests linked
to specific issues are in `test/regression`.

Each test file consists of an input and an expected output.
The input begins after a line `<<< FORMAT` and the output
begins after a line `>>> FORMAT`.

If many tests fail as a result of changes, but the test
failures are all because of improvements in the output,
you can pass `--accept` to the test suite (e.g., with
`--test-arguments=--accept` on `stack test`), and the existing
golden files will be overwritten.  If you do this, inspect
the outputs very carefully to make sure they are correct.

If you pass the `--roundtrip` option into the test suite
(e.g., using `--test-arguments=--roundtrip` with `stack test`),
round-trip tests will be run instead.  Many of these will
fail. In these tests, the native inputs in `test/roundtrip/*.native`
will be converted to (respectively) `mml`, `omml`, or `tex`,
then converted back, and the result will be compared with the
starting point.  Although we don't guarantee that this kind
of round-trip transformation will be the identity, looking
at cases where it fails can be a guide to improvements.

# Authors

John MacFarlane wrote the original TeX reader, MathML writer, Eq
writer, and OMML writer.  Matthew Pickering contributed the
MathML reader, the TeX writer, and many of the auxiliary
modules.  Jesse Rosenthal contributed the OMML reader.  Thanks
also to John Lenz for many contributions.
