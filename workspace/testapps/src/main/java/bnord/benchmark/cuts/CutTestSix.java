package bnord.benchmark.cuts;

import bnord.examples.Lock;

public class CutTestSix extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static CutTestSix thread = new CutTestSix();

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
		if (thread.myfield == 0)
			dummy(0);
		if (thread.myfield == 0)
			dummy(0);
		if (thread.myfield == 0)
			dummy(0);
		if (thread.myfield == 0)
			dummy(0);
		if (thread.myfield == 0)
			thread.myfield = 3;
		synchronized (lock1) {
			thread.start();
		}
		excludeMe();


	}

	void p2() {
		thread.myfield = 2;
		excludeMe();
		thread.myfield = 1;
		excludeMe();
		thread.myfield = 5;
		excludeMe();
		thread.myfield = 6;
		excludeMe();
		thread.myfield = 7;
		excludeMe();
	}
}
