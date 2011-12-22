package bnord.unittests.randomisolation;

public class BSP01 extends Thread{

	/**
	 * @param args
	 */
	int x;
	
	public static void main(String[] args) {
		for(int j = 0; j < 10; j ++ ) {
			BSP01 t = new BSP01();
			t.start();
		}
	}
	
	public void run() {
		go();
	}
	
	synchronized void go() {
		excludeMe();
	}
	
	public void excludeMe() {
		x = 42;
	}

}
