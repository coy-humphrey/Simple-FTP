import java.net.*;
import java.io.*;

public class ClientHandler implements Runnable
{
    class GetHandler implements Runnable
    {
        
    }
    
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
            while ((line = in.readLine()) != null)
            {
            	handleLine (line);
            }
        }
        catch (IOException e)
        {
        	System.out.println ("Failed to thread");
        }
	}

    public void handleLine (String line)
    {
        String[] words = line.split (" ");
        switch (words[0])
        {
            case "exit": client.close(); break;
            case "port": 
        }
    }
}