import UniversalLanguageInterface as ULI


main = do
    ULI.callInterpreter "runhaskell" "Callee.py" "haskellCallee" "Caller : Haskell -- "
