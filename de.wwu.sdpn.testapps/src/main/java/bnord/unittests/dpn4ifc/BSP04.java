package bnord.unittests.dpn4ifc;

public class BSP04 extends Thread {

	/**
	 * @param args
	 */
	static int x;

	public static void main(String[] args) {
		BSP04 p2 = new BSP04();
		p2.start();
		p2.p1();
	}

	public void run() {
		p2();
	}

	synchronized void p1() {
		x = 17;
		dummy(x); // instr. index 2
	}

	synchronized void p2() {
		x = 42; // instr. index 1
	}

	public static void dummy(int y) {
	}

}
