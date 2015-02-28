from twisted.internet import protocol, reactor, endpoints
from twisted.protocols.basic import LineReceiver, FileSender
import sys
import os

def trim_split (str):
    return str.strip().split()

class Answer(LineReceiver):
    def __init__(self):
        self.next_port = -1

    def lineReceived (self, line):
        functions = {
        "getQuote" : self._get_quote_,
        "echo"     : self._echo_,
        "get"      : self._get_,
        "port"     : self._port_,
        "dir"      : self._dir_,
        "exit"     : self._exit_,
        }
        cmd = trim_split (line)
        if len(cmd) < 1:
            return
        if functions.has_key (cmd[0]):
            functions[cmd[0]](cmd[1:])
        else:
            self.transport.write ("Invalid command\n")

    def _get_quote_ (self, cmd):
        self.transport.write ("Have a good day.\n")

    def _echo_ (self, cmd):
        line = " ".join(cmd)
        self.transport.write (line)

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
        endpoint = endpoints.serverFromString(reactor, "tcp:" + "0")




class AnswerFactory(protocol.Factory):
    protocol = Answer

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


# Receiving factory can just recv raw data =]
# Work like sending factory, take a file name
# open file and dump all raw data into file
'''class ReceivingProtocol(protocol.Protocol):
    def dataReceived(self, data):
        self.factory.fp.write (data)

class ReceivingFactory(protocol.Factory):
    protocol = ReceivingProtocol

    def __init__(self, fname):
        self.file = fname

    def startFactory(self):
        self.fp = open(self.file, 'bw')

    def stopFactory(self):
        self.fp.close()
'''
endpoints.serverFromString(reactor, "tcp:" + sys.argv[1]).listen(AnswerFactory())
reactor.run()
