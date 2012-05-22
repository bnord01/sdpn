package de.wwu.sdpn.core.util

class NullProgressMonitor extends IProgressMonitor {
    private var canceled = false

    override def beginTask(name: String, totalWork: Int): Unit = {}

    override def done(): Unit = {}

    override def isCanceled(): Boolean = { canceled }

    override def setCanceled(value: Boolean): Unit = { canceled = value }

    override def subTask(name: String): Unit = {}

    override def worked(work: Int): Unit = {}

    override def worked(work: Double): Unit = {}

}