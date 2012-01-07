#!/bin/sh
# Note: this should be run from within the tests directory
# Make sure you've set the 'test' flag using Cabal:
# cabal install -ftest
# Otherwise the test program 'texmath' won't be built.
# Exit status is number of failed tests.
TESTPROG=../dist/build/texmath/texmath
totalfailures=0

for format in xhtml omml; do
  echo "============="
  echo "Format ${format}"
  echo "============="
  failures=0
  passes=0
  if [ "$format" = "omml" ]; then
    inline="--inline"
  else
    inline=""
  fi
  if [ -f $TESTPROG ]; then
      for t in *.tex; do
          $TESTPROG --${format} ${inline} <$t >tmp
          if [ "$?" -ne "0" ]; then
            echo "Test ${t%.tex} FAILED"
            failures=`expr $failures + 1`
          else
            diff -u ${t%.tex}.${format} tmp >tmpdiff
            if [ "$?" -ne "0" ]; then
                echo "Test ${t%.tex} FAILED (- expected, + actual):"
                cat tmpdiff
                failures=`expr $failures + 1`
            else
                echo "Test ${t%.tex} PASSED"
                passes=`expr $passes + 1`
            fi
          fi
      done
  else
      echo "Test executable not built. NOT running tests."
  fi
  echo "$passes tests passed, $failures tests failed."
  totalfailures=`expr $totalfailures + $failures`
done

exit $failures


