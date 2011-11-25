package bnord.testapps;

public class DoubleMethodInvocation {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		methodA();
		methodB();
		methodA();
		if(random())
			methodB();
	}
	
	public static void methodA() {
		
	}
	public static void methodB() {
		
	}
	
	static boolean random() {
		return false;
	}

}
