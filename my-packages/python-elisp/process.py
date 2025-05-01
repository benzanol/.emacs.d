class ElispSymbol:
    name = None
    def __init__(self, name):
        self.name = name

class ElispBuffer:
    name = None
    def __init__(self, name):
        self.name = name


def elisp_eval(string):
    print("--eval--" + string)
    response = input()
    try:
        return eval(response)
    except:
        print(f"Couldn't evaluate response: `{response}`")
        return None

def elisp_repr(obj):
    if type(obj) == str:
        return '"' + obj + '"'
    elif type(obj) == list or type(obj) == tuple:
        return "(list " + " ".join(map(elisp_repr, obj)) + ")"
    elif type(obj) == ElispSymbol:
        return obj.name
    elif type(obj) == ElispBuffer:
        return f'(get-buffer "{obj.name}")'
    else:
        return repr(obj)

def elisp_call(func, *args):
    arg_str = " ".join(map(elisp_repr, args))
    eval_str = "(" + func + " " + arg_str + ")"
    return elisp_eval(eval_str)

def elisp_set(var, val):
    print(elisp_repr(val))
    elisp_eval(f"(setq {var} {elisp_repr(val)})")

while True:
    input_str = input()
    code_str = input_str.replace("\\n", "\n").replace("\\b", "\\")
    try:
        exec(code_str)
    except:
        condensed_code_str = code_str.replace('\n', '\\n')
        print(f"Error evaluating: `{condensed_code_str}`")

