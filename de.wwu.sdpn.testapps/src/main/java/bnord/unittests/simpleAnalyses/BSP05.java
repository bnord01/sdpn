package bnord.unittests.simpleAnalyses;

import bnord.examples.Lock;

public class BSP05 extends Thread {
	static void p1() {
		thread.start();         
	    synchronized(lock1){  
	        synchronized(lock2) { dummy(); }
	        excludeMe();
	    } 
	}

	void p2() {
		synchronized(lock2){
	        excludeMe();
	    } 
	}
	
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static BSP05 thread = new BSP05();


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