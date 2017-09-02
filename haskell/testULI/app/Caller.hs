import UniversalLanguageInterface as ULI


main = do
    ULI.callInterpreter "runhaskell" "Callee.hs" "haskellCallee" "Caller : Haskell -- "
