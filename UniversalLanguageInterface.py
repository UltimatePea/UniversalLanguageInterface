
import argparse
import json

export_list = {}
def export(f):
    if f.__name__ in export_list:
        raise ValueError("Function name clashes at {}".format(f.__name__))
    export_list[f.__name__] = f


def start_single_mode():
    info = json.loads(input())
    name = info["name"]
    args = info["args"]
    if name not in export_list:
        raise ValueError("Function {} does not exist".format(name))
    return_val({"code":200, "return_val":export_list[name](*args)})
    

    

def return_val(dic):
    print(json.dumps(dic))


def safe_start():
    # parse command line argument
    
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", default="single")

    args = parser.parse_args()

    if args.mode == "single":
        start_single_mode()
    elif args.mode == "stream":
        start_stream_mode()

        
def start():
    try:
        safe_start()
    except Exception as e:
        return_val({"code":500, "message":str(e)})
