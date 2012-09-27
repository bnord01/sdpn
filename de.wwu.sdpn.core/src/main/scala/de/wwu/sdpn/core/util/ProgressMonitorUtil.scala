package de.wwu.sdpn.core.util
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.ScheduledFuture
import scala.sys.ShutdownHookThread
import java.util.concurrent.ThreadFactory
import com.codahale.logula.Logging

object ProgressMonitorUtil extends Logging {

    /**
     * Sets the main task of this progress monitor.
     * This method should only be called once!
     */
    def beginTask(implicit pm: IProgressMonitor, name: String, totalWork: Int) {
        if (pm != null)
            pm.beginTask(name, totalWork)
    }

    def done(implicit pm: IProgressMonitor) {
        if (pm != null)
            pm.done
    }

    def isCanceled(implicit pm: IProgressMonitor): Boolean = {
        if (pm != null)
            return pm.isCanceled()
        else
            return false
    }

    def setCanceled(implicit pm: IProgressMonitor, value: Boolean) {
        if (pm != null)
            pm.setCanceled(value)
    }

    def subTask(implicit pm: IProgressMonitor, name: String) {
        if (pm != null)
            pm.subTask(name)
    }

    def worked(implicit pm: IProgressMonitor, work: Int) {
        if (pm != null)
            pm.worked(work)
    }

    /**
     * Wraps an Eclipse or Wala IProgressMonitor into a sDPN IProgressMonitor
     */
    def wrapPM(pm: Any): IProgressMonitor = {
        try {
            return new WPMWrapper(pm)
        } catch {
            case e: ClassNotFoundException   => return new EPMWrapper(pm)
            case e: IllegalArgumentException => return new EPMWrapper(pm)
        }
    }

    /**
     * Checks every '''delay''' ms whether the given progress monitor has been canceled and if so
     * the call back '''cb''' is called.
     * The thread can indicate that it want's to stop listening
     * by parsing the returned option to the '''delisten''' method.
     */
    def listenOnCancel(pm: IProgressMonitor, cb: () => _, delay: Int): Option[ScheduledFuture[_]] = {
        if (pm == null)
            None
        else
            Some(scheduler.scheduleWithFixedDelay(new Runnable {
                def run() {
                    if (pm.isCanceled()) {
                        try {
                            cb()
                        } finally {
                            throw new Exception("Unschedule the ugly way.")
                        }
                    }
                }
            }, 10, delay, TimeUnit.MILLISECONDS))
    }

    /**
     * Checks every '''delay''' ms whether the given progress monitor has been canceled
     * or the timeout has been exceeded and if so the call back '''cb''' is called.
     * The thread can indicate that it want's to stop listening
     * by parsing the returned option to the '''delisten''' method.
     */
    def listenOnCancel(timeout: Long, pm: IProgressMonitor, cb: () => _, delay: Int = 100): Option[ScheduledFuture[_]] = {
        val kill = if (timeout > 0) System.currentTimeMillis + timeout else Long.MaxValue
        Some(scheduler.scheduleWithFixedDelay(new Runnable {
            def run() {
                if ((pm != null && pm.isCanceled()) || kill <= System.currentTimeMillis) {
                    try {
                        if(kill <= System.currentTimeMillis) {
                            log.debug("Job exceeded given timeout, calling callback.")
                        } else {
                            log.debug("Progress monitor indicated canceling, calling callback.")
                        }
                        cb()
                    } finally {
                        throw new Exception("Unschedule the ugly way.")
                    }
                }
            }
        }, 10, delay, TimeUnit.MILLISECONDS))
    }

    def delisten(future: Option[ScheduledFuture[_]]) {
        future.foreach(_.cancel(false))
    }

    lazy private val scheduler = {
        val tf = new ThreadFactory {
            def newThread(r: Runnable): Thread = {
                val t = new Thread(r)
                t.setDaemon(true)
                t
            }
        }
        val res = Executors.newSingleThreadScheduledExecutor(tf)
        ShutdownHookThread({ res.shutdown() })
        res
    }

}