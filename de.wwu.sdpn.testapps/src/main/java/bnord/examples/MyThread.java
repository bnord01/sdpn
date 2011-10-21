package bnord.examples;

public class MyThread extends Thread{
	
	public static void main (String[] args) {
		createAndRun();
	}
	public static void createAndRun() {
		MyThread t = new MyThread();
		t.start();
	}
}
