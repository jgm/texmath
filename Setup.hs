import Distribution.Simple
import System.Process (system)
import System.Exit

main = defaultMainWithHooks $ simpleUserHooks { runTests  = runTestSuite }

runTestSuite _ _ _ _ = system "cd tests && ./runtests.sh" >>= exitWith
