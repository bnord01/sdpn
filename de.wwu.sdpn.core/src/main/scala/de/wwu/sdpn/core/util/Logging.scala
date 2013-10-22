package de.wwu.sdpn.core.util

trait Logging {
    val logger = new Object {
      def fatal(params: Any*): Unit = ()
      def warn(params: Any*): Unit = ()
      def debug(params: Any*): Unit = ()
      def log(params: Any*): Unit = ()
      def verbose(params: Any*): Unit = ()
      def trace(params: Any*): Unit = ()
      def error(params: Any*): Unit = ()
      def info(params: Any*): Unit = ()
      def isDebugEnabled = true
      def isTraceEnabled=true
    }
}