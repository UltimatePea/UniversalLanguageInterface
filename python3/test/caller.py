
import UniversalLanguageInterface as ULI


ret = ULI.callSingle("python3", ["callee.py"], "test_func", "Caller :: python3 -- ")
print(ret)

