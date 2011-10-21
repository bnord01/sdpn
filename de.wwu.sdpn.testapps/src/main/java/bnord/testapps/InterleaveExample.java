package bnord.testapps;

public class InterleaveExample extends Thread {

	public static void writeConsole(String s){
		for(char c : s.toCharArray()) {
			System.out.print(c);
			Thread.yield();			
		}
	}
	
	public static void main(String[] args) {
		new InterleaveExample().start();
		new InterleaveExample().start();

	}
	
	public synchronized void write() {
		writeConsole("12345");
	}
	
	public void run() {
		write();
	}

}
