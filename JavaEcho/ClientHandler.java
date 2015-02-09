import java.net.*;
import java.io.*;

public class ClientHandler implements Runnable
{
    Socket client;
	public ClientHandler (Socket c)
	{
        client = c;
	}

	public void run()
	{
		try (
    		PrintWriter out =
    		    new PrintWriter(client.getOutputStream(), true);
    		BufferedReader in = new BufferedReader(
    			new InputStreamReader(client.getInputStream()));
    		)
    	{
    		String line;
            while (!((line = in.readLine()).equals ("exit")))
            {
            	out.println(line);
            	System.out.println(line);
            }
        }
        catch (IOException e)
        {
        	System.out.println ("Failed to thread");
        }
	}
}