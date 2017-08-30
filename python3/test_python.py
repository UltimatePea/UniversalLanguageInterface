import os
import subprocess


os.system('cp UniversalLanguageInterface test/')
os.chdir('test')

output = subprocess.check_output('python3 caller.py')
assert output == ""



