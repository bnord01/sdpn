package de.wwu.sdpn.core.ta.xsb.reachability

import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import scala.collection.Set
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

class TwoSetConflictTA[StackSymbol<%HasTermRepresentation](override val name:String,cset1:Set[StackSymbol],cset2:Set[StackSymbol]) 
	extends IntersectionTA(new SingleSetConflictTA(name+"3",cset1.union(cset2)),
	 new IntersectionTA(new SingleSetReachingTA(name+"1",cset1),new SingleSetReachingTA(name+"2", cset2))) {

}