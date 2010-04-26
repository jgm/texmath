#!/bin/sh
# Note: this should be run from within the tests directory
# Make sure you've set the 'test' flag using Cabal:
# cabal install -ftest
# Otherwise the test program won't be built.
TESTPROG=../dist/build/testTeXMathML/testTeXMathML
for t in *.tex; do
    $TESTPROG <$t >tmp
    diff ${t%.tex}.xhtml tmp >tmpdiff
    if [ "$?" -ne "0" ]; then
        echo "Test ${t%.tex} FAILED (< expected, > actual):"
        cat tmpdiff
    else
        echo "Test ${t%.tex} PASSED"
    fi
done
