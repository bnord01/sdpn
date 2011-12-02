package de.wwu.sdpn.util

import scala.collection.mutable.HashSet
import com.ibm.wala.util.graph.Graph

import scala.collection.Iterator
import scala.collection.JavaConversions._

object EdgeIterator {
  
	def visitEdges[E](g:Graph[E],f:(E,E) => Unit) = {
		for (e <- g)
			for (e1 <- g.getSuccNodes(e))
				f(e,e1)		
	}	
}