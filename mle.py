import pylog
import time
import pyro
import torch
from torch.distributions import constraints
from pyro.infer import SVI, Trace_ELBO
from argparse import ArgumentParser

time.sleep(1)

pyro.enable_validation(True)

parser = ArgumentParser("LEARN")
parser.add_argument("--input", required=True, type=str)
parser.add_argument("--iterations", type=int, default=100)
parser.add_argument("--lr", type=float, default=0.01)

args = parser.parse_args()

print(f"Reading {args.input}...")
with open(args.input) as f:
    contents = f.read()

print(f"Parsing the contents...")
program, evidence, parameters = pylog.parse(contents)

print("Registering parsed program...")
pylog.register(program)

print("Evaluating data points...")
data = []
for query in evidence:
    model, observations = pylog.query(query)
    # we'll just grab the first observation rn
    data.append( (model, observations[0]) )

print("Preprocessing done.")

def model_parameters(params):
    result = {}
    for param_name in params:
        param = pyro.param(param_name, torch.tensor(0.5), constraint=constraints.unit_interval)
        result[param_name] = param
    return result

def model(data):
    params = model_parameters(parameters)
    for i in pyro.plate("data", len(data)):
        m, obs = data[i] # get the ith point
        c = pyro.condition(m.to_pyro, data=obs.indexed(i)) # condition the to_pyro method on the data
        c(params, index=i) # build the actual model

def guide(data):
    pass

def train(model, guide, lr=args.lr):
    pyro.clear_param_store()
    adam = pyro.optim.Adam({"lr" : lr})
    svi = SVI(model, guide, adam, loss=Trace_ELBO())

    for step in range(args.iterations):
        loss = svi.step(data)
        if step % 10 == 0:
            print(f"[iter {step}] loss: {loss:.04f}")

train(model, guide)

for value in parameters:
    print(f"MLE Estimate of {value}: {pyro.param(value).item():.3f}")