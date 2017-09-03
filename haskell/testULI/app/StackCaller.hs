import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callInterpreter "stack" ["exec", "--", "callee"] "haskellCallee" "Caller : Haskell -- "
    putStrLn x
