package bnord.unittests.simpleAnalyses;

import bnord.examples.Lock;

public class BSP03 extends Thread {
	static void p1() {
		synchronized (lock1) {
			thread.start();
			excludeMe();
		}
	}

	void p2() {
		synchronized (lock1) {
			dummy();
		}
		excludeMe();
	}

	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static BSP03 thread = new BSP03();

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

}