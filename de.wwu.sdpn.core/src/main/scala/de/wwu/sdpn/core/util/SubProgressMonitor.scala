package de.wwu.sdpn.core.util

class SubProgressMonitor(parent: IProgressMonitor, ticksToSend: Int) extends IProgressMonitor {
    private var scale = 0d
    private var ticksSent = 0d
    private var canceled = false;

    
    def beginTask(task: String, totalWork: Int) {
        if (parent == null)
            return
        parent.subTask(task)
        scale = if (totalWork <= 0) 0 else ticksToSend / totalWork.toDouble
    }

    def subTask(subTask: String) {
        if (parent == null)
            return
        parent.subTask(subTask)
    }

    def isCanceled: Boolean = {
        if (parent == null)
            canceled
        else
            parent.isCanceled()
    }

    def setCanceled(value: Boolean) {
        canceled = value
        if (parent != null)
            parent.setCanceled(value)

    }

    def done() {
        if (parent == null)
            return
        val toSent = ticksToSend - ticksSent
        ticksSent = ticksToSend
        if (toSent > 0)
            parent.worked(toSent)
    }

    def worked(work: Double) {
        if (parent == null)
            return
        val toSend = work * scale
        ticksSent += toSend
        parent.worked(work * scale)

    }
    
    def worked(units: Int) {
        worked(units.toDouble)
    }
}