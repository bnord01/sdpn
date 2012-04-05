package bnord.unittests.defuse;

public class Test05 extends Thread{	
	static int field;

	public static void main(String[] args) {
		Test05 other = new Test05();
		other.start();
		callMe1();		
	}
	
	public void run(){
		field = 42;
	}
	
	public static int readField() {
		return field;
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
		readField();
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
		readField();
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
		readField();
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
		readField();
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
		readField();
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
		readField();
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
		readField();
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
		readField();
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
		readField();
	}

	static boolean random() {
		return false;
	}

}
