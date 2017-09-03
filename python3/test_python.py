import os, sys, subprocess


# Change the directory to be this shell's execution directory
os.chdir(sys.path[0])

# TODO remove this
os.chdir('test')

output = subprocess.check_output(['python3', 'caller.py']).decode('utf-8')
print("Output is " + str(output))
assert output == "caller :: callee returned : Hello, World, Name!!\n"



