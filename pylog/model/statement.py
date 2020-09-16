from . import term
from . import distribution
import pyro

class Statement:
    def __init__(self, variable, dependencies, distribution):
        self.variable = variable
        self.dependencies = dependencies
        self.distribution = distribution

    def __str__(self):
        return f"{self.variable} ~ {self.distribution}"

    @classmethod
    def of_json(cls, json):
        variable = term.Variable(json["variable"])
        dependencies = [term.Variable(dep) for dep in json["dependencies"]]
        dist = distribution.of_json(json["distribution"])
        return cls(variable, dependencies, dist)

    def to_pyro(self, variable_mapping, parameter_mapping):
        dist = self.distribution.to_pyro(variable_mapping, parameter_mapping)
        return self.variable, pyro.sample(self.variable.name, dist)

def of_json(json):
    return Statement.of_json(json)