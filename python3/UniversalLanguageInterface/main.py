import argparse
import json
import os, tempfile, subprocess
import logging, sys

#logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)
logging.basicConfig(stream=sys.stderr, level=logging.INFO)

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
    logging.debug("Starting Single Mode")
    logging.debug("[Begin] Callee -- Opening Caller -> Callee Pipe ")
    with open(input_pipe_name) as inp:
        logging.debug("[Done] Callee -- Opening Caller -> Callee Pipe")
        line = inp.readline()
        info = json.loads(line)
        name = info["name"]
        args = info["args"]
        if name not in export_list:
            raise ValueError("Function {} does not exist".format(name))
        return_val(output_pipe_name, {"code":200, "return_val":export_list[name](args)})
    

    

def return_val(output_pipe_name, dic):
    """
    Open the output pipe and dump json represetation of dic as a line
    """
    logging.debug("[Begin] Callee -- Opening Callee -> Caller Pipe ")
    with open(output_pipe_name, 'w') as f:
        logging.debug("[Done] Callee -- Opening Callee -> Caller Pipe ")
        f.write(json.dumps(dic) + os.linesep)


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


class CallSetting:
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
    # write argument to `inp`, Open will block until the other end also opens, so need subprocess call
    logging.debug("[Begin] Forking off Sub process")
    subpro = subprocess.Popen([setting.prog, setting.filename, "--mode", "single", "--input-pipe", outpipe, "--output-pip", inpipe])
    logging.debug("[Done] Forking off Sub process")
    logging.debug("[Begin] Caller -- Opening Caller -> Callee Pipe ")
    with open(outpipe, 'w') as o:
        logging.debug("[Done] Caller -- Opening Caller -> Callee Pipe ")
        o.write(json.dumps({
            "name": setting.function_name,
            "args": setting.args
            }) + os.linesep)
    # call program in single mode
    
    # program should put one line to inpipe, we need to open pipe here in order to prevent the subprocess from blocking
    logging.debug("[Begin] Caller -- Opening Callee -> Caller Pipe ")
    with open(inpipe) as i:
        logging.debug("[Done] Caller -- Opening Callee -> Caller Pipe ")
    # wait for the process to finish
        logging.debug("[Begin] waiting for sub process to finish")
        subpro.wait()
        logging.debug("[Done] waiting for sub process to finish")
        line = i.readline()
        obj = json.loads(line)
        if obj["code"] == 500:
            raise ValueError("Callee Internal Error : " + obj["message"])
        elif obj["code"] == 200:
            return obj["return_val"]
        else:
            raise ValueError("Unable to understand response, no response code given")

    




def call_python3(filename, function_name, args):
    return call(python_call_setting("python3", filename, function_name, args))
