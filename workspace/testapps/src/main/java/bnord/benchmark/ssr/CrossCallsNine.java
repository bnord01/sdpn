package bnord.benchmark.ssr;

import bnord.examples.Lock;

public class CrossCallsNine extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static CrossCallsNine thread = new CrossCallsNine();

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
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
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
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
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
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}
	static void callMe4() {
		if (random()) {
			callMe1();
		} else if (random()) {
			callMe2();
		} else if (random()) {
			callMe3();
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}
	static void callMe5() {
		if (random()) {
			callMe1();
		} else if (random()) {
			callMe2();
		} else if (random()) {
			callMe3();
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}
	static void callMe6() {
		if (random()) {
			callMe1();
		} else if (random()) {
			callMe2();
		} else if (random()) {
			callMe3();
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}
	static void callMe7() {
		if (random()) {
			callMe1();
		} else if (random()) {
			callMe2();
		} else if (random()) {
			callMe3();
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}
	static void callMe8() {
		if (random()) {
			callMe1();
		} else if (random()) {
			callMe2();
		} else if (random()) {
			callMe3();
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}
	static void callMe9() {
		if (random()) {
			callMe1();
		} else if (random()) {
			callMe2();
		} else if (random()) {
			callMe3();
		} else if (random()) {
			callMe4();
		} else if (random()) {
			callMe5();
		} else if (random()) {
			callMe6();
		} else if (random()) {
			callMe7();
		} else if (random()) {
			callMe8();
		} else if (random()) {
			callMe9();
		}
		synchronized (lock1) {
			excludeMe();
		}
	}

	static boolean random() {
		return false;
	}

}