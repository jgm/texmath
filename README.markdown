texmath
=======

texmath is a Haskell library for converting between formats used to
represent mathematics.  Currently it provides functions to read and
write both TeX math and presentation MathML, and to write OMML (Office
Math Markup Language, used in Microsoft Office) and [pandoc]'s native
format (allowing conversion, using pandoc, to a variety of different
markup formats).  It supports basic LaTeX and AMS extensions, and it can
parse and apply LaTeX macros.  The package also includes several utility
modules which may be useful for anyone looking to manipulate either
Unicode or MathML.  For example, a copy of the MathML operator
dictionary is included.

[pandoc]: http://github.com/jgm/pandoc

You can [try it out online here](http://johnmacfarlane.net/texmath.html).
(Note that the math it produces will be rendered correctly only
if your browser supports MathML. Firefox does; Safari and Chrome do not.)

By default, only the Haskell library is installed.  To install a
test program, `texmath`, use the `executable` Cabal flag:

    cabal install -fexecutable

To run the test suite, compile with `--enable-tests` and do `cabal test`.

`texmath` reads a LaTeX formula from stdin and writes a
standalone xhtml file to stdout.  You can run the test suite thus:

    cd tests
    sh runtests.sh

Macro definitions may be included before the formula.

`texmath` will behave as a CGI script when called under the name
`texmath-cgi` (e.g. through a symbolic link).

The file `cgi/texmath.html` contains an example of how it can
be used.

Thanks to Matthew Pickering and John Lenz for many contributions.

