import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callInterpreter "runhaskell" ["HaskellCallee.hs"] "testFunc" "Caller :: Haskell -- "
    putStrLn x
