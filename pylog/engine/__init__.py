from .socket import JSONSocket
from ..config import PORT
from . import server

def echo(message):
    with JSONSocket(PORT) as s:
        s.send(message)
        response = s.receive()
    return response