import UniversalLanguageInterface as ULI


main = do
    x <- ULI.callSingle "runhaskell" ["HaskellCallee.hs"] "testFunc" "Caller :: Haskell -- "
    putStrLn x
