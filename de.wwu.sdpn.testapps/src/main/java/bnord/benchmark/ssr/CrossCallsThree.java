package bnord.benchmark.ssr;

import bnord.examples.Lock;

public class CrossCallsThree extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static CrossCallsThree thread = new CrossCallsThree();

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
		callMe1();
	}

	void p2() {
		callMe1();
	}

	static void callMe1() {
		if (random()) {
				callMe1();
		} else if (random()) {
				callMe2();
		} else if (random()) {
				callMe3();
		}
		synchronized(lock1){
			excludeMe();
		}

	}
	static void callMe2() {
		if (random()) {
				callMe1();
		} else if (random()) {
				callMe2();
		} else if (random()) {
				callMe3();
		}
		synchronized(lock1){
			excludeMe();
		}

	}
	static void callMe3() {
		if (random()) {
				callMe1();
		} else if (random()) {
				callMe2();
		} else if (random()) {
				callMe3();
		}
		synchronized(lock1){
			excludeMe();
		}

	}

	static boolean random() {
		return false;
	}
		
	
}