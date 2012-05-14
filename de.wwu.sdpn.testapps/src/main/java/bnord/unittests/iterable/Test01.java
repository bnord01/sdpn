package bnord.unittests.iterable;

public class Test01 extends Thread {

	static int x;
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		x = 1;
		synchronized(Test01.class) {
			x = 2;
		}
		x = 3;
		excludeMe();
	}
	
	public static void excludeMe() {
		
	}
	
	

}
