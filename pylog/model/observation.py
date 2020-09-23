from . import term
import torch

class Observation:
    def __init__(self, observations):
        self.observations = observations

    def __str__(self):
        args = (f"{k}/{v}" for k, v in self.observations.items())
        return f"[{', '.join(args)}]"

    @classmethod
    def of_json(cls, json):
        observations = {entry["variable"] : term.of_json(entry["value"]) for entry in json}
        return cls(observations)

    def indexed(self, index):
        result = {}
        for k, v in self.observations.items():
            k_i = f"{k}_{index}"
            result[k_i] = torch.tensor(v)
        return result


def of_json(json):
    return Observation.of_json(json)