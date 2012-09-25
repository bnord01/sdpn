package bnord.unittests.dpn4ifc;

public class BSP09 extends Thread {

	/**
	 * @param args
	 */
	int x;

	public static void main(String[] args) {
		while(true) {
			BSP09 p2 = new BSP09();
			p2.start();
			p2.p1();
		}
	}

	public void run() {
		p2();
	}

	synchronized void p1() {
		killX();
		dummy(x); // instr. index 3
	}

	synchronized void p2() {
		x = 42;  // instr. index 2
	}

	public static void dummy(int y) {
	}
	void killX() {
		x = 17;
	}

}
