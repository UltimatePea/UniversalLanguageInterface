
import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callSingle "node" ["--use-strict", "nodejs_callee.js"] "test_func" "Caller :: Haskell -- "
    putStrLn x
