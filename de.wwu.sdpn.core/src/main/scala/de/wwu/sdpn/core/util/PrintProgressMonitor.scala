package de.wwu.sdpn.core.util

import java.io.PrintStream

class PrintProgressMonitor(out:PrintStream) extends IProgressMonitor {
    def this() = {
        this(System.out)
    }
	private var first = true
	private var canceled = false
	
	
	private def nl {
	    if (!first) {
	        out.println
	    } else {
	        first = true
	    }
	}
    
    def beginTask(name: String, totalWork: Int): Unit = {
        nl
        out.print(name + " ( " + totalWork + " ) ")
    }

    def done(): Unit = {out.println("done!")}

    def isCanceled(): Boolean = { canceled }

    def setCanceled(value: Boolean): Unit = {canceled = value}

    def subTask(name: String): Unit = {nl; out.print(name + " ")}

    def worked(work: Int): Unit = {out.print(".")}
    
    def worked(work: Double): Unit = {out.print(".")}

}