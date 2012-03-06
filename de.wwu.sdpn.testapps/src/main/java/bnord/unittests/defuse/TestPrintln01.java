package bnord.unittests.defuse;

public class TestPrintln01 extends Thread{	
	static int field;

	public static void main(String[] args) {
		TestPrintln01 other = new TestPrintln01();
		other.start();
	}
	
	public void run(){
		field = 42;
		System.out.println(field);
	}

}
