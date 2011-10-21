package bnord.examples;

import bnord.examples.Lock;

public class BSP02 extends Thread {
	static void p1() {
		thread.start();
		synchronized (lock1) {
			excludeMe();
		}
	}

	void p2() {
		synchronized (lock1) {
			excludeMe();
		}
	}

	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static BSP02 thread = new BSP02();

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