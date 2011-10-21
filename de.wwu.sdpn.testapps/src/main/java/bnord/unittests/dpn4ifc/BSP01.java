package bnord.unittests.dpn4ifc;

import bnord.examples.Lock;

public class BSP01 extends Thread{

	/**
	 * @param args
	 */
	static int x;
	static Lock lock1 = new Lock();
	
	public static void main(String[] args) {
		p1();
	}
	
	public void run() {
		p2();
	}
	
	static void p1() {
		BSP01 p2 = new BSP01();
		synchronized(lock1) {
			p2.start();
			dummy(x);
		}
	}
	static void p2() {
		synchronized(lock1) {
			x = 42;
		}
	}
	
	public static void dummy(int y) {}

}
