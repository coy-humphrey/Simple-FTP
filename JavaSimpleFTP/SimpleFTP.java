import java.net.*;
import java.io.*;

public class SimpleFTP
{
    public static void main (String args[]) throws IOException 
    {
        if (args.length != 1)
        {
        	System.err.println("Usage: java SimpleFTP <port>");
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
        	System.err.println ("Failed to bind to listen port");
        }
    }

    public static void handleConnection (ServerSocket ss)
    {
    	Socket clientSocket = null;
        try
        {
        	clientSocket = ss.accept();
        }
        catch (IOException e){}
        (new Thread(new ClientHandler(clientSocket))).start();
    }
}