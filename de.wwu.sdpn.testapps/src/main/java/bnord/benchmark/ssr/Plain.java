package bnord.benchmark.ssr;

import bnord.examples.Lock;

public class Plain extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Plain thread = new Plain();


	public static void main(String[] args) {
		p1();
	}

	public void run() {
		p2();
	}

	static void excludeMe() {
	}

	static void dummy() {
	}

	static void p1() {
		thread.start();         
	    synchronized(lock1){  
	        synchronized(lock2) { dummy(); }
	        excludeMe();
	    } 
	}

	void p2() {
		synchronized(lock2){  
	        synchronized(lock1) { dummy(); }
	        excludeMe();
	    } 
	}
}