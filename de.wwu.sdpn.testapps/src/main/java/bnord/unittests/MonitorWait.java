package bnord.unittests;

import bnord.examples.Lock;

public class MonitorWait {

	static Lock lock1 = new Lock();
	
    public static void main(String [] args) throws InterruptedException{
            synchronized(lock1) {
                   lock1.wait();
                   excludeMe();
            }

    }

    static void excludeMe() {

    }

}