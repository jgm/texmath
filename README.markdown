texmath
=======

texmath is a Haskell library for converting between formats used to
represent mathematics.  Currently it provides functions to read and
write TeX math, presentation MathML, and OMML (Office Math Markup
Language, used in Microsoft Office), and to write [pandoc]'s native
format (allowing conversion, using pandoc, to a variety of different
markup formats).  The TeX reader and writer supports basic LaTeX and AMS
extensions, and it can parse and apply LaTeX macros.  The package also
includes several utility modules which may be useful for anyone looking
to manipulate either TeX math or MathML.  For example, a copy of the
MathML operator dictionary is included.

[pandoc]: http://github.com/jgm/pandoc

You can [try it out online here](http://johnmacfarlane.net/texmath.html).
(Note that the math it produces will be rendered correctly only
if your browser supports MathML. Firefox does; Safari and Chrome do not.)

By default, only the Haskell library is installed.  To install a
test program, `texmath`, use the `executable` Cabal flag:

    cabal install -fexecutable

To run the test suite, compile with `--enable-tests` and do `cabal test`.

Macro definitions may be included before a LaTeX formula.

`texmath` will behave as a CGI script when called under the name
`texmath-cgi` (e.g. through a symbolic link).

The file `cgi/texmath.html` contains an example of how it can
be used.

Generating lookup tables
=======================

There are three main lookup tables which are built form externally compiled lists.
This section contains information about how to modify and regenerate these tables.

In the `lib` direction there are two sub-directories which contain the
necessary files.

MMLDict.hs
----------

The utility program `xsltproc` is required.
You can find these files in `lib/mmldict/`

  1. If desired replace `unicode.xml` with and updated version (you can download a copy from [here](http://www.w3.org/TR/xml-entity-names/#source)
  2. `xsltproc -o dictionary.xml operatorDictionary.xsl unicode.xml`
  3. `runghc generateMMLDict.hs`
  4. Replace the operator table at the bottom of `src/Text/TeXMath/Readers/MathML/MMLDict.hs` with the contents of `mmldict.hs`

ToTeXMath.hs
------------

You can find these files in `lib/totexmath/`

  1. If desired, replace `unimathsymbols.txt` with an updated verson from [here](http://milde.users.sourceforge.net/LUCR/Math/)
  2. `runghc unicodetotex.hs`
  3. Replace the record table at the bottom of `src/Text/TeXMath/Unicode/ToTeXMath.hs` with the contents of `UnicodeToLaTeX.hs`

ToUnicode.hs
------------

You can find these files in `lib/tounicode/`.

  1. If desired, replace `UnicodeData.txt` with an updated verson from
     [here](ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt).
  2. `runghc mkUnicodeTable.hs`
  3. Replace the table at the bottom of
     `src/Text/TeXMath/Unicode/ToUnicode.hs` with the output.

Editing the tables
==================

It is not necessary to edit the source files to add records to the tables.
To add to or modify a table it is easier to add modify either `unicodetotex.hs`
or `generateMMLDict.hs`. This is easily achieved by adding an item to the corresponding
`updates` lists. After making the changes, follow the above steps to regenerate
the table.

Authors
=======

John MacFarlane wrote the original TeX reader, MathML writer, and
OMML writer.  Matthew Pickering contributed the MathML reader, the TeX
writer, and many of the auxiliary modules.  Jesse Rosenthal contributed
the OMML reader.  Thanks also to John Lenz for many contributions.
