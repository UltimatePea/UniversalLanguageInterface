
import function_ref


#ret = ULI.call_python3("callee.py", "b", ["Name"])
ret = function_ref.b("Name")
print("caller :: callee returned : " + ret)

