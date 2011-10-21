package bnord.unittests;

import bnord.examples.Lock;

public class Main extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static Main thread = new Main();

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
			try {
				lock1.wait();
			} catch (InterruptedException e) {
				
			}
			excludeMe();
		}
	}

	void p2() {
		synchronized (lock1) {
			dummy(0);
		}

		excludeMe();

		if (myfield == 0)
			thread.myfield = 3;
	}
}