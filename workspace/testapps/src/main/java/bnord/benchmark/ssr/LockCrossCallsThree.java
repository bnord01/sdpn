package bnord.benchmark.ssr;

import bnord.examples.Lock;

public class LockCrossCallsThree extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static LockCrossCallsThree thread = new LockCrossCallsThree();

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

	static void p1() {
		thread.start();
		callMe();
	}

	void p2() {
		callMe();
	}

	static void callMe() {
		if (random()) {
			synchronized (lock1) {
				callMe();
			}
		} else if (random()) {
			synchronized (lock2) {
				callMe();
			}
		}else if (random()) {
			synchronized (lock3) {
				callMe();
			}
		}
		synchronized(lock1){
			excludeMe();
		}

	}

	static boolean random() {
		return false;
	}
}