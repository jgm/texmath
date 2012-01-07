#!/bin/sh
# Note: this should be run from within the tests directory
# Make sure you've set the 'test' flag using Cabal:
# cabal install -ftest
# Otherwise the test program 'texmath' won't be built.
# Exit status is number of failed tests.
TESTPROG=../dist/build/texmath/texmath
totalfailures=0

for format in xhtml omml; do
  failures=0
  passes=0
  if [ -f $TESTPROG ]; then
      for t in *.tex; do
          $TESTPROG --${format} <$t >tmp
          testname="Test ${t%.tex} ${format}"
          if [ "$?" -ne "0" ]; then
            echo "${testname} FAILED"
            failures=`expr $failures + 1`
          else
            diff -u ${t%.tex}.${format} tmp >tmpdiff
            if [ "$?" -ne "0" ]; then
                echo "${testname} (- expected, + actual):"
                cat tmpdiff
                failures=`expr $failures + 1`
            else
                echo "${testname} PASSED"
                passes=`expr $passes + 1`
            fi
          fi
      done
  else
      echo "Test executable not built. NOT running tests."
  fi
  echo "$format: $passes tests passed, $failures tests failed."
  totalfailures=`expr $totalfailures + $failures`
done
echo "$totalfailures tests failed total."
exit $failures


