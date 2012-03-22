package bnord.unittests.defuse;

public class Test03 extends Thread{	
	static int field;
	int field2;
	public static void main(String[] args) {
		Test03 other = new Test03();
		other.start();
		readFields(other);		
	}
	
	public void run(){
		field = 42;
		this.field2 = 3;
	}
	
	public static int readFields(Test03 inst) {
		return field + inst.field2;
	}

}
