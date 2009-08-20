#!/bin/sh
TESTPROG=../dist/build/testTeXMathML/testTeXMathML
for t in *.tex; do
    $TESTPROG <$t >tmp
    diff ${t%.tex}.xhtml tmp >tmpdiff
    if [ "$?" -ne "0" ]; then
        echo "Test $t failed (< expected, > actual):"
        cat tmpdiff
    else
        echo "Test $t passed."
    fi
done
