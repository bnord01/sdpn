package bnord.unittests.dpn4ifc;

public class BSP06 extends Thread {

	/**
	 * @param args
	 */
	int x;

	public static void main(String[] args) {
		BSP06 p2 = new BSP06();
		p2.start();
		p2.p1();
	}

	public void run() {
		p2();
	}

	synchronized void p1() {
		dummy(x); // instr. index 1
	}

	// This kills the value 42 after writing, there are exceptions here in
	// WALA, even though it's the this pointer.
	synchronized void p2() {
		x = 42;  // instr. index 2
		x = 17;
	}

	public static void dummy(int y) {
	}

}
