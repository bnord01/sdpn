package bnord.examples;

import bnord.examples.Lock;

public class CutTest extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static CutTest thread = new CutTest();

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

		thread.myfield = 3;

		if (thread.myfield == 0)
			dummy(0);

	}

	void p2() {

		excludeMe();

	}
}
