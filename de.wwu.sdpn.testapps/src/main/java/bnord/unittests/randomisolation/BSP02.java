package bnord.unittests.randomisolation;

public class BSP02 extends Thread{

	/**
	 * @param args
	 */
	int x;
	
	public static void main(String[] args) {
		for(int j = 0; j < 10; j ++ ) {
			BSP02 t = new BSP02();
			t.start();
			t.excludeMe();
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
