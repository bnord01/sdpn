package sdpn.ta.prolog.reachability

import sdpn.ta.IntersectionTA
import sdpn.dpn.explicit.StackSymbol
import scala.collection.Set

class TwoSetConflictTA(override val name:String,cset1:Set[StackSymbol],cset2:Set[StackSymbol]) 
	extends IntersectionTA(new SingleSetConflictTA(name+"3",cset1.union(cset2)),
	 new IntersectionTA(new SingleSetReachingTA(name+"1",cset1),new SingleSetReachingTA(name+"2", cset2))) {

}