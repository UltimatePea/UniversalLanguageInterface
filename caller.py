
import UniversalLanguageInterface as ULI


ret = ULI.call_python3("callee.py", "b", ["Name"])
print("caller :: callee returned : " + ret)

