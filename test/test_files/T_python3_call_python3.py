
import UniversalLanguageInterface as ULI


ret = ULI.callSingle("python3", ["python3_callee.py"], "test_func", "Caller :: python3 -- ")
print(ret)

