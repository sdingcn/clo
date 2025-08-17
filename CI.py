import json
import os
import os.path
import subprocess
import sys
import time
from typing import List, Tuple, Union

def execute(cmd: List[str], i: Union[None, str] = None) -> Tuple[int, str, str]:
    result = subprocess.run(
        cmd,
        text = True,
        input = i,
        capture_output = True
    )
    return (result.returncode, result.stdout, result.stderr)

def build(build_type: str) -> None:
    print(f"building the project ... ({build_type}) ... ", end = "", flush = True)
    start = time.time()
    res = execute(["make", "-C", "src/", build_type])
    end = time.time()
    if res[0]:
        sys.exit(f"make failed\nresult = {res}")
    print(f"OK ({end - start:.3f} seconds)", flush = True)

def check_copy_controls(err: str) -> bool:
    start = """\
[debug]constructor
[debug]copy constructor
[debug]move constructor
[debug]constructor
[debug]copy assignment operator
[debug]constructor
[debug]move assignment operator
"""
    end = """\
[debug]destructor
[debug]destructor
[debug]destructor
[debug]destructor
[debug]destructor
"""
    # there may be calls to `.eval` so we only check the first layer of states
    return err.startswith(start) and err.endswith(end)

def test() -> None:
    for dirpath, _, filenames in os.walk("test/"):
        for filename in filenames:
            if filename.endswith(".clo"):
                filepath = os.path.join(dirpath, filename)
                print(f"running test {filepath} ... ", end = "", flush = True)
                if filename.startswith("quine"):  # special treatment of quine tests
                    print("[[[quine]]] ... ", end = "", flush = True)
                    start = time.time()
                    res = execute(["bin/clo", filepath])
                    end = time.time()
                    with open(filepath, "r") as f:
                        src = f.read()
                    if (
                        res[0] == 0 and
                        res[1] == src + "<end-of-stdout>\n( VV )\n" and
                        check_copy_controls(res[2])
                    ):
                        print(f"OK ({end - start:.3f} seconds)", flush = True)
                    else:
                        sys.exit(f'test failed\nresult = {res}\n')
                else:
                    iopath = filepath[:-3] + "json"
                    with open(iopath, "r") as f:
                        io = json.loads(f.read())
                    start = time.time()
                    res = execute(["bin/clo", filepath], io["in"])
                    end = time.time()
                    if (
                        res[0] == 0 and
                        res[1] == io["out"] and
                        check_copy_controls(res[2])
                    ):
                        print(f"OK ({end - start:.3f} seconds)", flush = True)
                    else:
                        sys.exit(f'test failed\nresult = {res}\n')

if __name__ == "__main__":
    build("debug")
    test()
    print("passed all tests")
