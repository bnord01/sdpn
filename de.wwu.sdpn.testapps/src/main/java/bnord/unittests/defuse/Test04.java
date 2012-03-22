package bnord.unittests.defuse;

public class Test04 extends Thread{	
	static int field;
	int field2;
	public static void main(String[] args) {
		Test04 other = new Test04();
		other.start();
		readField();		
	}
	
	public void run(){
		field = 42;
		field++;
	}
	
	public static int readField() {
		return field;
	}

}
