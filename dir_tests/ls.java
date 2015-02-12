import java.io.File;

public class ls
{
	public static void main (String args[])
	{
		File directory = new File(".");
		File[] files = directory.listFiles();

		for (File f : files)
		{
			System.out.println (f.getName());
		}
	}
}