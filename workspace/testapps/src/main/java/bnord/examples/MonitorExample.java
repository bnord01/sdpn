package bnord.examples;


public class MonitorExample {
	Lock a = new Lock();
	Lock b = new Lock();	
	
	public void nestedMonitor(){
		synchronized (a) {
			synchronized (b) {
				someMethod();				
			}
		}
	}
	public void singleMonitor(){
		synchronized (a) {
			someMethod();				
		}
	}
	
	public synchronized void syncMethod() {
		someMethod();
	}
	
	private int someMethod () {return 0;}	
}
