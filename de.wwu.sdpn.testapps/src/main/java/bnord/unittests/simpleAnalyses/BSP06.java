package bnord.unittests.simpleAnalyses;

import bnord.examples.Lock;

public class BSP06 extends Thread {
	static void p1() {
		thread.start();         
	    synchronized(lock1){  
	        synchronized(lock2) { dummy(); }
	        synchronized(lock3) { dummy(); }
	        synchronized(lock4) { dummy(); }
	        synchronized(lock5) { dummy(); }
	        synchronized(lock6) { dummy(); }
	        synchronized(lock7) { dummy(); }
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
	static Lock lock4 = new Lock();
	static Lock lock5 = new Lock();
	static Lock lock6 = new Lock();	
	static Lock lock7 = new Lock();
	
	static BSP06 thread = new BSP06();
	


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