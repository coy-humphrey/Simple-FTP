import java.net.*;
import java.io.*;

public class ClientHandler implements Runnable
{
	//*********************************************************//
    //********************GET HANDLER**************************//
    //*********************************************************//
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
    
    //*********************************************************//
    //*******************LIST HANDLER**************************//
    //*********************************************************//
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
        			out.write(f.getName());
        		}
        	}
            catch (Exception e)
            {
                System.err.println ("Failed to connect to client");
            }
            port = 0;
        }
    }
    
    //*********************************************************//
    //********************PUT HANDLER**************************//
    //*********************************************************//
    class PutHandler implements Runnable
    {
        PrintWriter out;
        String file;
        
        public PutHandler(PrintWriter o, String file){
        	this.file = file;
        	this.out = o;
        }
        
        public void run(){
        	
        	int d;
        	try (
                ServerSocket sock = new ServerSocket(0);
        		
        		
                ) 
            {
            	out.println("" + sock.getLocalPort());
            	try (
            			Socket client = sock.accept();
                		InputStreamReader in = new InputStreamReader(client.getInputStream());
            			FileOutputStream fileout = new FileOutputStream(file);
            		) {
            		while ((d = in.read()) != -1){
            			fileout.write(d);
            		}
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
            	handleLine (out, line);
            }
        }
        catch (IOException e)
        {
        	System.err.println ("Failed to thread");
        }
	}

    public void handleLine (PrintWriter out, String line)
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
            	if (words.length != 2){
            		out.println("get invalid command");
            		break;
            	}
            	(new Thread(new GetHandler(client.getInetAddress(), port, words[1]))).start();
            	break;
            case "put":
            	if (words.length != 2){
            		out.println("put invalid command");
            		break;
            	}
            	(new Thread(new PutHandler(out, words[1]))).start();
            	break;
            case "port":
            	handlePort(words);
            	break;
            case "dir":
            	(new Thread(new ListHandler(client.getInetAddress(), port))).start();
            	break;
            case "cd":
            	if (words.length != 2){
            		out.println("cd invalid command");
            		break;
            	}
            	changeDirectory(words[1]);
            	break;
        }
    }    
    
    public void handlePort (String[] atgs){
    	port = Integer.parseInt(atgs[1]);
    }
    
    public boolean changeDirectory(String fn){
    	boolean result = false;
    	File directory;
    	
    	directory = new File(fn).getAbsoluteFile();
    	if (directory.exists() || directory.mkdirs()){
    		result = (System.setProperty("user.dir", directory.getAbsolutePath()) != null);
    	}
    	
    	return result;
    }
}