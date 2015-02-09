import java.net.*;
import java.io.*;

// See: http://docs.oracle.com/javase/tutorial/networking/sockets/readingWriting.html

public class SimpleFTP
{
    public static void main (String args[]) throws IOException 
    {
        if (args.length != 1)
        {
        	System.err.println("Usage: java EchoServer <port>");
        	System.exit(1);
        }

        int portNum = Integer.parseInt (args[0]);

        try (
        	ServerSocket serverSocket =
        	    new ServerSocket(portNum);
        	)
        {
        	for (;;)
        	{
        		handleConnection (serverSocket);
        	}
        }
        catch (IOException e)
        {
        	System.out.println ("couldn't listen on port");
        }
    }

    public static void handleConnection (ServerSocket ss)
    {
    	Socket clientSocket = null;
        try
        {
        	clientSocket = ss.accept();
        }
        catch (IOException e)
        {
        	System.out.println ("problem connecting to client");
        }
        (new Thread(new ClientHandler(clientSocket))).start();
    }
}