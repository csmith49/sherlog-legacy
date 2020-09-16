from . import term
import pyro

class Distribution:
    def __init__(self, distribution, arguments):
        self.distribution = distribution
        self.arguments = arguments

    def __str__(self):
        args = (f"{k}={v}" for k, v in self.arguments.items())
        return f"{self.distribution}[{', '.join(args)}]"

    @classmethod
    def of_json(cls, json):
        distribution = json["distribution"]
        arguments = {k : term.of_json(v) for k, v in json["arguments"].items()}
        return cls(distribution, arguments)

    def make_params(self, variable_mapping, parameter_mapping):
        # constructs parameters from the provided variable values
        params = {}
        for k, v in self.arguments.items():
            if isinstance(v, term.Variable):
                params[k] = variable_mapping[v.name]
            elif isinstance(v, str):
                params[k] = parameter_mapping[v]
            else:
                params[k] = v
        return params

    def to_pyro(self, variable_mapping, parameter_mapping):
        params = self.make_params(variable_mapping, parameter_mapping)
        return translate_distribution(self.distribution, **params)

def of_json(json):
    return Distribution.of_json(json)

class UnknownDistribution(Exception):
    pass

def translate_distribution(distribution, **kwargs):
    if distribution == "Normal":
        return pyro.distributions.Normal(kwargs["mean"], kwargs["sd"])
    elif distribution == "Bernoulli":
        return pyro.distributions.Bernoulli(kwargs["success"])
    else:
        raise UnknownDistribution()