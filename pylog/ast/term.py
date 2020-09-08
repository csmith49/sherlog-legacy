from enum import Enum

Type = Enum("Type", "INTEGER FLOAT BOOLEAN CONSTANT VARIABLE FUNCTION")

class Term:
    def __init__(self, type, value, *args):
        self.type = type
        self.value = value
        self.arguments = list(args)

    def to_json(self):
        pass

    @classmethod
    def of_json(cls, dict):
        if dict["type"] == "integer":
            return cls(Type.INTEGER, dict["value"])
        elif dict["type"] == "float":
            return cls(Type.FLOAT, dict["value"])
        elif dict["type"] == "boolean":
            return cls(Type.BOOLEAN, dict["value"])
        elif dict["type"] == "constant":
            return cls(Type.CONSTANT, dict["value"])
        elif dict["type"] == "variable":
            return cls(Type.VARIABLE, dict["value"])
        elif dict["type"] == "function":
            args = [Term.of_json(arg) for arg in dict["arguments"]]
            return cls(Type.FUNCTION, dict["value"], *args)
        else:
            raise NotImplementedError