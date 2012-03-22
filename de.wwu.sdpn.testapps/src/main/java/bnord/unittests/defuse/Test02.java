package bnord.unittests.defuse;

public class Test02 extends Thread{	
	static int field;

	public static void main(String[] args) {
		Test02 other = new Test02();
		other.start();
		readField();		
	}
	
	public void run(){
		field = 42;
	}
	
	public static int readField() {
		return field;
	}

}
