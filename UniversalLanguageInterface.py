import argparse
import json
import os, tempfile, subprocess

export_list = {}

def export(f):
    """
    Expose to ULI a function to call

    the name is to be the function's name
    
    Argument f: any function

    """


    if f.__name__ in export_list:
        raise ValueError("Function name clashes at {}".format(f.__name__))
    export_list[f.__name__] = f


def start_single_mode(input_pipe_name, output_pipe_name):

    """
    Start the one-shot mode

    reads input pipe one line
    result produced to output pipe

    """
    line = open(input_pipe_name).readline()
    info = json.loads(line)
    name = info["name"]
    args = info["args"]
    if name not in export_list:
        raise ValueError("Function {} does not exist".format(name))
    return_val({"code":200, "return_val":export_list[name](*args)})
    

    

def return_val(output_pipe_name, dic):
    """
    Open the output pipe and dump json represetation of dic as a line
    """
    with open(output_pipe_name) as f:
        f.writeline(json.dumps(dic))


def safe_start():
    # parse command line argument
    
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", default="single")
    parser.add_argument("--input-pipe", required=True)
    parser.add_argument("--output-pipe", required=True)

    args = parser.parse_args()

    if args.mode == "single":
        try:
            start_single_mode(args.input_pipe, args.output_pipe)
        except Exception as e:
            return_val(args.output_pipe, {"code":500, "message":str(e)})
    elif args.mode == "stream":
        start_stream_mode()

        
def start():
    safe_start()


class CallSettings:
    def __init__(self, prog, filename, function_name, args):
        self.prog = prog
        self.filename = filename
        self.function_name = function_name
        self.args = args
        self.mode = "single"


def python_call_setting(python_exec_name, filename, function_name, args):
    return CallSetting(python_exec_name, filename, function_name, args)


def call(setting):
    tempdir = tempfile.mkdtemp()
    outpipe = os.path.join(tempdir, "inp") # Caller to callee pipe
    inpipe = os.path.join(tempdir, "outp") # Callee to caller pipe
    os.mkfifo(outpipe) # TODO security hazard, handle OSError here
    os.mkfifo(inpipe)
    # write argument to `inp`
    with open(outpipe) as o:
        o.writeline(json.dump({
            "name": setting.function_name,
            "args": setting.args
            }))
    # call program in single mode
    subprocess.call([setting.prog, setting.filename, "--mode", "single", "--input-pipe", outpipe, "--output-pip", inpipe])
    # program should put one line to inpipe
    with open(inpipe) as i:
        line = i.readline()
        obj = json.loads(line)
        if obj["code"] == 500:
            raise ValueError("Callee Internal Error")
        elif obj["code"] == 200:
            return obj["return_val"]
        else:
            raise ValueError("Unable to understand response, no response code given")

    




def call_python3(filename, function, args):
    call(python_call_setting("python3", filename, function_name, args))
