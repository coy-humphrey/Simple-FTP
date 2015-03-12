% Simplified File Transfer Protocol
% Examining Networking Capabilities of Haskell, Java, and the Event-driven Python Library Twisted
% Coy Humphrey and Dustin Pfeiffer


Project Outline
---

Our goal was to create an FTP server in different languages to compare the networking capabilities of each language, as well as
the ease of programming and understanding. Our project is inspired by the File Transfer Protocol, but it is not an exact copy.
As in FTP, our protocol involves a telnet port and separate data ports for file transfer.

Our protocol includes the following commands:

*  `get` *filename* - Used to get files from the server. The port command must be called before calling
`get`. The server will attempt to connect to the client on the port specified by the earlier `port`
command. If the connection is successful, the server will send the entire file through the new 
connection and close the connection when the file is finished.
*  `put` *filename* - Used to send files to the server. The server will respond with a string containing 
the port number to connect to over the telnet socket.The client should open a connection to the server
on this port and send the contents of the file. When the file has been sent the connection should be 
closed.
*  `port` *portNum* - Used to specify the port the server should connect to during the next `get` command.
*  `dir` - Used to see the contents of the directory the server is running in. The server sends this
information as a string over the telnet socket.
*  `exit` - Used to end the connection. The server responds by closing the telnet socket.

We chose to implement our project in Java, Haskell, and Python using a library called Twisted.

Why choose these languages?
---

We chose Java because we already had an understanding of Java and we feel it represents a prototypical imperative language.
Similarly, we chose Haskell because we were learning it in class, and we feel it provides a good representation of functional
programming languages. We chose Twisted to explore event-driven develop of a network application.

Handling Clients
---
### Java

~~~ {.java}
SimpleFTP.java
for (;;) {
    Socket clientSocket = serverSocket.accept();
    (new Thread(new ClientHandler(clientSocket))).start();
}
~~~

In Java, we had a main loop that would constantly accept clients and start a new thread to
handle each new client. 

### Haskell
~~~ {.haskell}
loop serv_sock = do 
    (h,host,_) <- accept serv_sock
    forkIO $ handler h host ""
    loop serv_sock
~~~

The Haskell implementation follows the same structure as the one in Java. Notably, while 
Haskell does not have an infinite for loop as in Java, it can still loop endlessly by using
tail recursion. Additionally, Haskell can start a new thread using only a function, rather than creating a Thread object as in Java.

### Python
~~~ {.python}
please put da python code with the def and the if and the elif in here
~~~

Handling Files
---

### Java

~~~ {.java}
ClientHandler.java
while ((int d = in.read()) != -1){
    fileout.write(d);
}
~~~

Our Java implementation is naive method of reading and writing files. It reads and writes
a single byte at a time without buffering, until it reaches the end of the file. This affects
speed of the transfer, but is still suitable for our goals in this project.

### Haskell

~~~ {.haskell}
withFile file ReadMode (\handle -> do
    contents <- B.hGetContents handle
    B.hPut sock contents)
~~~

Our Haskell implementation takes advantage of Haskell's laziness. Conceptually, we read in the
contents of the entire file, then write everything to the socket. Haskell will handle any
buffering that needs to be done.

### Python

~~~ {.python}
def connectionMade(self):
    fs = FileSender()
    fs.beginFileTransfer(self.factory.fp, self.transport)
~~~

Twisted provides a class called FileSender for sending files.

Code Length
---

| Language | File               | Lines    | Total |
|:---------|:-------------------|---------:|------:|
| Java     | SimpleFTP.java     | 152      |       |
|          | ClientHandler.java | 42       | 194   |
| Haskell  | SimpleFTP.hs       | 82       | 82    |
| Python   | SimpleFTP.py       | 102      | 102   |

Java was the most verbose of the languages. It consists of four classes over two files.
The SimpleFTP class contains the main method. ClientHandler contains two inner classes to
handle the `get` and `put` commands. The files, however, are riddled with `try`/`catch`
blocks, which make the code that does the work difficult to find.

Haskell was the shortest program. However, what it gains in brevity, it loses in clarity.
It is by far the most dense, and the code must be read closely to gain a good understanding
of how it works under the hood.

Twisted, with just over a hundred lines of code, can still be difficult to understand. Because
it is event-based, it requires a closer reading than just following the code, as you can do
in imperative languages, such as Java.

Parsing Input
---

After a client connects to the server, they are able to send commands. The server parses these commands
and performs an appropriate action. The code to do the parsing was remarkably similar between the three
languages. Each implementation involves splitting the command string into words, then checking the first
word for the command. An appropriate function is called with the remaining words passed in as arguments.
The cleanest of these implementations was done in python and is shown below.

~~~ {.python}
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
~~~

Threading Differences
---
### Java

In the Java program, the ClientHandler class also had two inner classes, a GetHandler and
a PutHandler, which implement Runnable so each command could be threaded as well. We had to 
pass the client's IP, port number, and the file name given to each Handler, so objects that
required these could be constructed, as Java has no clean way of sharing variables between
classes.

~~~ {.java}
ClientHandler.java
class GetHandler implements Runnable{
    InetAddress addr;
    int portNum;
    String file;

    public GetHandler(InetAddress addr, int portNum, String file){
        this.addr = addr;
        this.portNum = portNum;
        this.file = file;
    }
    
    public void run(){
        ...
    }
}
~~~

The run method carried out the specified command, sending a file from the client or server to the
other. It is called when the Thread is created, and when completed, the Thread terminates.

The only notable difference between the GetHandler and the PutHandler are the directions of the
reading or file streams. `get` has a FileInputStream to read from, whereas `put` has a
FileOutputStream to write to.

### Haskell

Threading is Haskell is much cleaner. The variables can be passed directly through to the new
function call as parameters, and no Thread objeect needs to be created.

~~~ {.haskell}
(h,host,_) <- accept sock
forkIO $ handler handle host ""
~~~

The socket's handle and the host IP are passed to a function `handler`, which executes commands
that the client inputs.
