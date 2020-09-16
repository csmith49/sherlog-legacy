from .socket import JSONSocket
from ..config import PORT
from ..model import Model, Observation
from . import server

def echo(obj):
    message = {
        "command" : "echo",
        "message" : obj
    }
    with JSONSocket(PORT) as s:
        s.send(message)
        response = s.receive()
    return response

def parse(string):
    message = {
        "command" : "parse",
        "message" : string
    }
    with JSONSocket(PORT) as s:
        s.send(message)
        response = s.receive()
    return response["program"], response["query"]

def register(program):
    message = {
        "command" : "register",
        "message" : program
    }
    with JSONSocket(PORT) as s:
        s.send(message)

def query(q):
    message = {
        "command" : "query",
        "message" : q
    }
    with JSONSocket(PORT) as s:
        s.send(message)
        response = s.receive()
    return Model.of_json(response["model"]), [Observation.of_json(obs) for obs in response["observations"]]