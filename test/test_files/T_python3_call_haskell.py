
import UniversalLanguageInterface as ULI


ret = ULI.callSingle("runhaskell", ["HaskellCallee.hs"], "testFunc", "Caller :: python3 -- ")
print(ret)

