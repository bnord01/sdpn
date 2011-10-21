package bnord.testapps;

public class MonitorWait extends Thread {

    public static void main(String [] args) throws InterruptedException{
            MonitorWait a = new MonitorWait();
            synchronized(a) {
                    a.start();
                    a.wait();
                    excludeMe();
            }

    }

    public void run() {
            synchronized(this){
                    notifyAll();
            }
            excludeMe();
    }

    static void excludeMe() {

    }

}