texmath
=======

texmath is a Haskell library for converting LaTeX math to
MathML.  It is used by [pandoc] and [gitit].

[pandoc]: http://github.com/jgm/pandoc
[gitit]: http://gitit.net

You can [try it out online here](http://johnmacfarlane.net/texmath.html).
(Note that the math it produces will be rendered correctly only
if your browser supports MathML. Firefox does; Safari and Chrome do not.)

By default, only the Haskell library is installed.  To install a
test program, `texmath`, use the `test` Cabal flag:

    cabal install -ftest

`texmath` reads a LaTeX formula from stdin and writes a
standalone xhtml file to stdout.  You can run the test suite thus:

    cd tests
    sh runtests.sh

Macro definitions may be included before the formula.

The `cgi` Cabal flag will cause a CGI binary, `texmath-cgi`, to be
produced:

    cabal install -fcgi

The file `cgi/texmath.html` contains an example of how it can
be used.

Thanks to John Lenz for many contributions.

