import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callInterpreter "runhaskell" ["Callee.hs"] "haskellCallee" "Caller : Haskell -- "
    putStrLn x
