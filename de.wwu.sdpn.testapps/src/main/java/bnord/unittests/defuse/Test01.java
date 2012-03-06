package bnord.unittests.defuse;

public class Test01 extends Thread{	
	static int field;

	public static void main(String[] args) {
		Test01 other = new Test01();
		other.start();
	}
	
	public void run(){
		field = 42;
	}

}
