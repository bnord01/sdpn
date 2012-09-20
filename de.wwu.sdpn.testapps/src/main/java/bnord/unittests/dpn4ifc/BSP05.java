package bnord.unittests.dpn4ifc;

public class BSP05 extends Thread {

	/**
	 * @param args
	 */
	static int x;

	public static void main(String[] args) {
		BSP05 p2 = new BSP05();
		p2.start();
		p2.p1();
	}

	public void run() {
		p2();
	}

	synchronized void p1() {
		dummy(x); // instr. index 0
	}

	// This kills the value 42 after writing, there are no exceptions here in
	// WALA most likely because it's a static field.
	synchronized void p2() {
		x = 42;  // instr. index 1
		x = 17;
	}

	public static void dummy(int y) {
	}

}
