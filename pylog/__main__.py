from engine import register, parse, query
from argparse import ArgumentParser
import pyro

parser = ArgumentParser()
parser.add_argument("--input", type=str, required=True)

args = parser.parse_args()

with open(args["input"], "r") as f:
    contents = f.read()

program, evidence, parameters = parse(contents)

register(program)

for observation in evidence:
    print(observation)