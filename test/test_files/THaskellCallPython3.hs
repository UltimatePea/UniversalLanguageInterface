
import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callInterpreter "python3" ["python3_callee.py"] "test_func" "Caller :: Haskell -- "
    putStrLn x
