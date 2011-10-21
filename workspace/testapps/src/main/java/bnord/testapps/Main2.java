package bnord.testapps;

import java.io.IOException;

import bnord.examples.Lock;


public class Main2 extends Thread {
	static Lock lock1 = new Lock();
	static Lock lock2 = new Lock();
	static Lock lock3 = new Lock();
	static Lock lock4 = new Lock();
	static Lock lock5 = new Lock();
	static Lock lock6 = new Lock();
	static Lock lock7 = new Lock();
	static Lock lock8 = new Lock();

	public static void newThread() {
		Thread a = new Main2();
		a.start();
	}

	// static String x ;

	public static void main(String[] args) throws IOException {
		Thread a = new Main2();

		excludeMe();
		a.start();

		synchronized (lock1) {
			synchronized (lock2) {
				synchronized (lock3) {
					synchronized (lock4) {
						synchronized (lock5) {
							synchronized (lock6) {
								synchronized (lock7) {
									synchronized (lock8) {
										dummy();
									}
								}
							}
						}
					}
				}
			}
			excludeMe();
		}

	}

	public void run() {
		synchronized (lock2) {
			synchronized (lock3) {
				synchronized (lock4) {
					synchronized (lock5) {
						synchronized (lock6) {
							synchronized (lock7) {
								synchronized (lock8) {
									synchronized (lock1) {
										dummy();
									}
								}
							}
						}
					}
				}
			}
			excludeMe();
		}
	}

	public static void excludeMe() {

	}

	public static void dummy() {
	};

}