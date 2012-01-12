package de.wwu.sdpn.core.util

trait IProgressMonitor {

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
  def beginTask(name: String, totalWork: Int): Unit

  /**
   * Notifies that the work is done; that is, either the main task is completed
   * or the user canceled it. This method may be called more than once
   * (implementations should be prepared to handle this case).
   */
  def done(): Unit

  /**
   * Internal method to handle scaling correctly. This method
   * must not be called by a client. Clients should
   * always use the method </code>worked(int)</code>.
   *
   * @param work the amount of work done
   */
  def internalWorked(work: Double): Unit

  /**
   * Returns whether cancelation of current operation has been requested.
   * Long-running operations should poll to see if cancelation
   * has been requested.
   *
   * @return <code>true</code> if cancellation has been requested,
   *    and <code>false</code> otherwise
   * @see #setCanceled(boolean)
   */
  def isCanceled(): Boolean

  /**
   * Sets the cancel state to the given value.
   *
   * @param value <code>true</code> indicates that cancelation has
   *     been requested (but not necessarily acknowledged);
   *     <code>false</code> clears this flag
   * @see #isCanceled()
   */
  def setCanceled(value: Boolean): Unit

  /**
   * Sets the task name to the given value. This method is used to
   * restore the task label after a nested operation was executed.
   * Normally there is no need for clients to call this method.
   *
   * @param name the name (or description) of the main task
   * @see #beginTask(java.lang.String, int)
   */
  def setTaskName(name: String): Unit

  /**
   * Notifies that a subtask of the main task is beginning.
   * Subtasks are optional; the main task might not have subtasks.
   *
   * @param name the name (or description) of the subtask
   */
  def subTask(name: String)

  /**
   * Notifies that a given number of work unit of the main task
   * has been completed. Note that this amount represents an
   * installment, as opposed to a cumulative amount of work done
   * to date.
   *
   * @param work a non-negative number of work units just completed
   */
  def worked(work: Int): Unit

}