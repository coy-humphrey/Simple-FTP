from twisted.internet import protocol, reactor, endpoints
from twisted.protocols.basic import LineReceiver, FileSender
import sys
import os

def trim_split (str):
    return str.strip().split()

# Quick and dirty hack found at:
# http://code.activestate.com/recipes/531822-pick-unused-port/
import socket

def PickUnusedPort():
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.bind(('localhost', 0))
  addr, port = s.getsockname()
  s.close()
  return port

# Protocol that receives a line at a time from the client,
# parses each line and performs an appropriate action.
class Answer(LineReceiver):
    def __init__(self):
        self.next_port = -1

    def lineReceived (self, line):
        cmd = trim_split (line)
        if len(cmd) < 1:
            return
        functions = {
            "get"      : self._get_,
            "port"     : self._port_,
            "dir"      : self._dir_,
            "put"      : self._put_,
            "exit"     : self._exit_,
        }
        if functions.has_key (cmd[0]):
            functions[cmd[0]](cmd[1:])
        else:
            self.transport.write ("Invalid command\n")

    def _exit_ (self, cmd):
        self.transport.loseConnection()

    def _port_ (self, cmd):
        self.next_port = int(cmd[0])

    def _get_ (self, cmd):
        if self.next_port < 0:
            self.transport.write ("Set port before calling get")
            return
        reactor.connectTCP (self.transport.getPeer().host, self.next_port, SendingFactory(cmd[0]))
        self.next_port = -1

    def _dir_ (self, cmd):
        listing = os.listdir('.')
        listing_str = "\n".join(listing)
        self.transport.write (listing_str + "\n")

    def _put_ (self, cmd):
        port = PickUnusedPort()
        endpoint = endpoints.serverFromString(reactor, "tcp:" + str(port))
        endpoint = endpoint.listen (ReceivingFactory(cmd[0], endpoint))
        self.transport.write (str(port) + '\n')


class AnswerFactory(protocol.Factory):
    protocol = Answer

# Sending protocol just sends a file as soon as a connection is made
class SendingProtocol(protocol.Protocol):
    def connectionMade(self):
        def f1(lastChunk):
            print "finished"
            self.transport.loseConnection()

        def f2(reason):
            print "failed"
            print reason
            self.transport.loseConnection()

        fs = FileSender()
        fs.beginFileTransfer(self.factory.fp, self.transport, None).addCallbacks(f1, f2)

class SendingFactory(protocol.Factory):
    protocol = SendingProtocol

    def __init__(self, fname):
        self.file = fname

    def startFactory(self):
        self.fp = open(self.file,'rb')

    def stopFactory(self):
        print "closing factory"
        self.fp.close()

    def startedConnecting(self, c):
        print "Connecting"
    def clientConnectionLost (self,a,b):
        print "disconnected"
    def clientConnectionFailed (self,a=None,b=None):
        print "Failed to connect"


# Receiving factory can just recv raw data
# Work like sending factory, take a file name
# open file and dump all raw data into file
class ReceivingProtocol(protocol.Protocol):
    def dataReceived(self, data):
        self.factory.fp.write (data)

    def connectionLost(self, a):
        print "stopping protocol"
        self.factory.stopFactory()

class ReceivingFactory(protocol.Factory):
    protocol = ReceivingProtocol

    def __init__(self, fname, endpoint):
        self.file = fname
        self.endpoint = endpoint

    def startFactory(self):
        self.fp = open(self.file, 'wb')

    def stopFactory(self):
        print "stoppping factory"
        self.fp.close()
        self.endpoint.addCallback(stopListening)

endpoints.serverFromString(reactor, "tcp:" + sys.argv[1]).listen(AnswerFactory())
reactor.run()
