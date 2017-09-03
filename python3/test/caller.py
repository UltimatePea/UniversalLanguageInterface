
import UniversalLanguageInterface as ULI


ret = ULI.callSingle("python3", ["callee.py"], "b", "Name")
print("caller :: callee returned : " + ret)

