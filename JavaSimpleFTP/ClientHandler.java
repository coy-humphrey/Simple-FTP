import java.net.*;
import java.io.*;

public class ClientHandler implements Runnable
{
	
    class GetHandler implements Runnable
    {
        InetAddress addr;
        int portNum;
        String file;
        
        public GetHandler(InetAddress addr, int portNum, String file){
        	this.addr = addr;
        	this.portNum = portNum;
        	this.file = file;
        }
        
        public void run(){
        	
        	int d;
        	try (
                Socket sock = new Socket(addr, portNum);
                PrintWriter out =
                    new PrintWriter(sock.getOutputStream(), true);
                FileInputStream in =
                    new FileInputStream(file);
                ) 
            {
            	while ((d = in.read()) != -1)
                {
            		out.write(d);
            	}
        	}
            catch (Exception e)
            {
                System.err.println ("Failed to connect to client");
            }
            port = 0;
        }
    }
    
    class ListHandler implements Runnable{
    	InetAddress addr;
        int portNum;
        
        public ListHandler(InetAddress addr, int portNum){
        	this.addr = addr;
        	this.portNum = portNum;
        }
        
        public void run(){
        	
        	int d;
        	try (
                Socket sock = new Socket(addr, portNum);
                PrintWriter out =
                    new PrintWriter(sock.getOutputStream(), true);
        		
                ) 
            {
        		File dir = new File(".");
        		File[] dirs = dir.listFiles();
        		
        		for (File f : dirs){
        			out.write(f.getName() + "\n");
        		}
        	}
            catch (Exception e)
            {
                System.err.println ("Failed to connect to client");
            }
            port = 0;
        }
    }
    
    Socket client;
    int port;
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
        	System.err.println ("Failed to thread");
        }
	}

    public void handleLine (String line)
    {
        String[] words = line.split (" ");
        switch (words[0])
        {
            case "exit": try { client.close(); }
            			 catch (Exception e) {
            				System.out.println("No");
            			 }
            			 break;
            case "get":
            	(new Thread(new GetHandler(client.getInetAddress(), port, words[1]))).start();
            	break;
            case "port":
            	handlePort(words);
            	break;
            case "dir":
            	(new Thread(new ListHandler(client.getInetAddress(), port))).start();
            	break;
        }
    }    
    
    public void handlePort (String[] atgs){
    	port = Integer.parseInt(atgs[1]);
    }
}