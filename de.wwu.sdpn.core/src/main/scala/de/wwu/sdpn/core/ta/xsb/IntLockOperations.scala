package de.wwu.sdpn.core.ta.xsb

class IntLockOperations(val numLocks: Int, val name: String = "ilo") extends LockOperations {
  require(numLocks < 9, "IntLockOperations can handle at most 8 locks!")

  override def genScript: String = {

    val buf = new StringBuilder()
    import buf.{ append => out }

    def outln(s: String) { out(s); out("\n") }
    for (i <- 0 until numLocks) {
      outln(name + "_lock(" + i + ").")
    }
    out("\n")
    out(
      """
%Helper functions for bitvector operations
name_isElemUnion(Elem,X,EX) :- EX is X '\/' (1 '<<' Elem).
name_disjoint(X,Y) :- 0 is X '/\' Y.
name_notin(Elem,X) :- 0 is X '/\' (1 '<<' Elem).
name_isNoElem(Elem,X) :- 0 is X '/\' (1 '<<' Elem).
name_isElem(Elem,X) :- not(0 is X '/\' (1 '<<' Elem)).
name_isUnion(X,Y,XY) :- XY is X '\/' Y.
name_isGraphUnion(X,Y,XY) :- XY is X '\/' Y.            
name_isGraphXUUnion(X,U,G,GUX) :- GUX is G '\/' (U '<<' (numLocks * X)).
name_isGraphXUUnion2(X,U,G1,G2,GUX) :- GUX is G1 '\/' (G2 '\/' (U '<<' (numLocks * X))).            
name_isElemUnion2(Elem,X,Y,EXY) :- EXY is X '\/' (Y '\/' (1 '<<' Elem)).
name_isUnion3(X,Y,Z,XYZ) :- XYZ is X '\/' (Y '\/' Z).
name_emptyGraph(0).
name_emptyLockSet(0).
        
:-table(name_edge/3).
name_edge(G,X,Y) :- name_lock(X), name_lock(Y), not(0 is (G '/\' (1 '<<' (Y + (numLocks * X))))).

:-table(name_path/3).
name_path(G,X,Y) :- name_edge(G,X,Y).
name_path(G,X,Z) :- name_path(G,X,Y),name_edge(G,Y,Z).

name_hasCycle(G) :- name_lock(X),name_path(G,X,X).        
name_notCyclic(G) :- not(name_hasCycle(G)).            
""".replace("numLocks", numLocks.toString).replace("name", name))
    return buf.mkString
  }

}