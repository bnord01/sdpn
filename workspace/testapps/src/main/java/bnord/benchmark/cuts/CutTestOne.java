package bnord.benchmark.cuts;

import bnord.examples.Lock;

public class CutTestOne extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static CutTestOne thread = new CutTestOne();

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
		synchronized (lock1) {
			thread.start();
		}
		excludeMe();
		
		if (thread.myfield == 0)
			dummy(0);
		thread.myfield = 3;

	}

	void p2() {

		excludeMe();

	}
}
