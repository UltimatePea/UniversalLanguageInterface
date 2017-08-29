
import UniversalLanguageInterface as ULI

def b(a:str):
    return "Hello, World, {}!!".format(a)

ULI.export(b)
ULI.start()
