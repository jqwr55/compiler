import os
import subprocess

def replace_last(source_string, replace_what, replace_with):
    head, _sep, tail = source_string.rpartition(replace_what)
    return head + replace_with + tail

for file in os.listdir("./tests"):
        if file.endswith(".q"):

            outBin = replace_last("./tests/"+file,".q",".bin")
            print("running " + outBin)
            subprocess.run(["./comp","--in","./tests/"+file,"--out",outBin])
            subprocess.run(["./virtual_machine","--in",outBin,"--show-vm-status","--jit-compile"])

            print("running " + outBin + " in -O1")
            subprocess.run(["./comp","--in","./tests/"+file,"--out",outBin,"-O1"])
            subprocess.run(["./virtual_machine","--in",outBin,"--show-vm-status","--jit-compile"])