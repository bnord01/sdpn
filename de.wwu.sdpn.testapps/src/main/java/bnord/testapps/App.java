package bnord.testapps;

/**
 * Hello world!
 *
 */
public class App 
{
	static protected Object x = null;
    public static void main( String[] args ) throws InterruptedException
    {
    	App a = new App();
    	a.x = "3";
    	App.x = "4";
    }
}
