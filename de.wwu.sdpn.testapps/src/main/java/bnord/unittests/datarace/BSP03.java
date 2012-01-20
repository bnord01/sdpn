package bnord.unittests.datarace;

import bnord.examples.Lock;

public class BSP03 extends Thread{

	/**
	 * @param args
	 */
	static int x;
	
	public static void main(String[] args) {
		BSP03 p2 = new BSP03();
		p2.p1();
	}
	
	public void run() {
		p2();
	}
	
	synchronized void p1() {
			this.start();
			dummy(x);
	}
	synchronized void p2() {	
			x = 42;
	}
	
	public static void dummy(int y) {}

}
