package de.wwu.sdpn.core.util
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.ScheduledFuture

object ProgressMonitorUtil {
  /**
   * Notifies that the main task is beginning.  This must only be called once
   * on a given progress monitor instance.
   *
   * @param name the name (or description) of the main task
   * @param totalWork the total number of work units into which
   *  the main task is been subdivided. If the value is <code>UNKNOWN</code>
   *  the implementation is free to indicate progress in a way which
   *  doesn't require the total number of work units in advance.
   */
  def beginTask(implicit pm: IProgressMonitor, name: String, totalWork: Int) {
    if (pm != null)
      pm.beginTask(name, totalWork)
  }

  /**
   * Notifies that the work is done; that is, either the main task is completed
   * or the user canceled it. This method may be called more than once
   * (implementations should be prepared to handle this case).
   */
  def done(implicit pm: IProgressMonitor) {
    if (pm != null)
      pm.done
  }

  /**
   * Internal method to handle scaling correctly. This method
   * must not be called by a client. Clients should
   * always use the method </code>worked(int)</code>.
   *
   * @param work the amount of work done
   */
  def internalWorked(implicit pm: IProgressMonitor, work: Double) {
    if (pm != null)
      pm.internalWorked(work)
  }

  /**
   * Returns whether cancelation of current operation has been requested.
   * Long-running operations should poll to see if cancelation
   * has been requested.
   *
   * @return <code>true</code> if cancellation has been requested,
   *    and <code>false</code> otherwise
   * @see #setCanceled(boolean)
   */
  def isCanceled(implicit pm: IProgressMonitor): Boolean = {
    if (pm != null)
      return pm.isCanceled()
    else
      return false
  }

  /**
   * Sets the cancel state to the given value.
   *
   * @param value <code>true</code> indicates that cancelation has
   *     been requested (but not necessarily acknowledged);
   *     <code>false</code> clears this flag
   * @see #isCanceled()
   */
  def setCanceled(implicit pm: IProgressMonitor, value: Boolean) {
    if (pm != null)
      pm.setCanceled(value)
  }

  /**
   * Sets the task name to the given value. This method is used to
   * restore the task label after a nested operation was executed.
   * Normally there is no need for clients to call this method.
   *
   * @param name the name (or description) of the main task
   * @see #beginTask(java.lang.String, int)
   */
  def setTaskName(implicit pm: IProgressMonitor, name: String) {
    if (pm != null)
      pm.setTaskName(name)
  }

  /**
   * Notifies that a subtask of the main task is beginning.
   * Subtasks are optional; the main task might not have subtasks.
   *
   * @param name the name (or description) of the subtask
   */
  def subTask(implicit pm: IProgressMonitor, name: String) {
    if (pm != null)
      pm.subTask(name)
  }

  /**
   * Notifies that a given number of work unit of the main task
   * has been completed. Note that this amount represents an
   * installment, as opposed to a cumulative amount of work done
   * to date.
   *
   * @param work a non-negative number of work units just completed
   */
  def worked(implicit pm: IProgressMonitor, work: Int) {
    if (pm != null)
      pm.worked(work)
  }

  /**
   * Wraps an Eclipse or Wala IProgressMonitor into a sDPN IProgressMonitor
   * @param pm
   * @return
   */
  def wrapPM(pm: Any): IProgressMonitor = {
    try {
      return new WPMWrapper(pm)
    } catch {
      case e: ClassNotFoundException => return new EPMWrapper(pm)
      case e: IllegalArgumentException => return new EPMWrapper(pm)
    }
  }

  def listenOnCancel(pm: IProgressMonitor, cb: () => _, delay: Int): ScheduledFuture[_] = {
    if (pm == null)
      null
    else
      scheduler.scheduleWithFixedDelay(new Runnable {
        def run() {
          if (pm.isCanceled()) {
            try {
              cb()
            } finally {
              throw new Exception("Unschedule the ugly way.")
            }
          }
        }
      }, 10, delay, TimeUnit.MILLISECONDS)
  }

  def delisten(future: ScheduledFuture[_]) {
    if (future != null)
      future.cancel(false)
  }

  lazy private val scheduler = Executors.newScheduledThreadPool(1)

}

trait CancelListener {
  def hasBeenCanceled(event: CancelEvent)
}
case class CancelEvent(progressMonitor: IProgressMonitor, task: String)