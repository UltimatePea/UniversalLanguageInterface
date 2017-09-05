
import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callSingle "python3" ["python3_callee.py"] "test_func" "Caller :: Haskell -- "
    putStrLn x
