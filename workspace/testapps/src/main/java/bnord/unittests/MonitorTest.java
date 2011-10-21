package bnord.unittests;

import bnord.examples.Lock;

public class MonitorTest {

	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	
    public static void main(String [] args) {
            nestedMonitors();

    }

    static void nestedMonitors() {
		synchronized(lock1) {
			synchronized(lock2){
				excludeMe();
			}
		}
		
	}

	static void excludeMe() {

    }

}