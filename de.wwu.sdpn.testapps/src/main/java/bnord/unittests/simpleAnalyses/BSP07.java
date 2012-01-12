package bnord.unittests.simpleAnalyses;


public class BSP07 extends Thread {
	static synchronized void p1() {
		thread.start();	
	    excludeMe();
	}

	static synchronized void p2() {
	        excludeMe();
	}
	
	
	
	static BSP07 thread = new BSP07();
	


	public static void main(String [] args) {
		p1();
	}

	public void run() {
		p2();
	}

	static void excludeMe() {
	}

	static void dummy() {
	}	
}