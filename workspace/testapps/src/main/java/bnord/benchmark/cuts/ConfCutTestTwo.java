package bnord.benchmark.cuts;

import bnord.examples.Lock;

public class ConfCutTestTwo extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static ConfCutTestTwo thread = new ConfCutTestTwo();

	int myfield = 0;

	public static void main(String[] args) {
		p1();
	}

	public void run() {
		p2();
	}

	static void excludeMe() {
	}

	static void dummy(int x) {
	}

	static void p1() {
		if (thread.myfield == 0)
			dummy(0);
		synchronized (lock1) {
			dummy(0);
		}
		thread.start();
		if (thread.myfield == 0)
			dummy(0);		
		excludeMe();		

	}

	void p2() {
		synchronized(lock1){
			excludeMe();
		}
		thread.myfield = 3;

	}
}
