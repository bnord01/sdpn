package bnord.unittests.dpn4ifc;

public class BSP08 extends Thread {

	/**
	 * @param args
	 */
	int x;

	public static void main(String[] args) {
		while(true) {
			BSP08 p2 = new BSP08();
			p2.start();
			p2.p1();
		}
	}

	public void run() {
		p2();
	}

	synchronized void p1() {
		x = 17;
		dummy(x); // instr. index 4
	}

	synchronized void p2() {
		x = 42;  // instr. index 2
	}

	public static void dummy(int y) {
	}

}
