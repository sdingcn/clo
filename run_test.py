import json
import os
import os.path
import shutil
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
    print("building the project ... ", end = "")
    sys.stdout.flush()
    start = time.time()
    code, _, _, = execute(["make", "-C", "src/", build_type])
    if code:
        sys.exit("make failed")
    end = time.time()
    print(f"OK ({end - start:.3f} seconds)")

def test() -> None:
    for dirpath, _, filenames in os.walk("test/"):
        for filename in filenames:
            if filename.endswith(".clo"):
                filepath = os.path.join(dirpath, filename)
                print(f"running test {filepath} ... ", end = "")
                sys.stdout.flush()
                iopath = filepath[:-3] + "json"
                with open(iopath, "r") as f:
                    io = json.loads(f.read())
                start = time.time()
                res = execute(["bin/clo", filepath], io["in"])
                end = time.time()
                if (res[0] == 0 and res[1] == io["out"]):
                    print(f"OK ({end - start:.3f} seconds)")
                else:
                    sys.exit(f'failed\nresult = {res}\n')

if __name__ == "__main__":
    print("# started testing debug version")
    build("debug")
    test()
    print("# started testing release version")
    build("release")
    test()
    print("passed all tests")
