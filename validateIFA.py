import sys
import subprocess
import json

tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/SLcorpus/"
test_sentence = "M40K/ASPEX/M40K1VI1D"

output = subprocess.check_output(["perl", "-I", tens_path + "scripts", tens_path + "scripts/ValidateSegmentation.pl", tens_path + "Labels/validation_tim/" + test_sentence + "*"])

print(json.loads(output))
