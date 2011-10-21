package bnord.examples;

import bnord.examples.Lock;

public class BSP01 extends Thread {
	static void p1() {
		excludeMe();
		thread.start();
	}

	void p2() {
		excludeMe();
	}

	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static BSP01 thread = new BSP01();

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