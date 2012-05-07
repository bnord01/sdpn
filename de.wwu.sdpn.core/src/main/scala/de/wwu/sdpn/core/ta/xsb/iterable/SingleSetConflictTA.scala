package de.wwu.sdpn.core.ta.xsb.iterable
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

/**
 * An tree automaton checking that at least two processes have reached some nil(X) nodes for an X in cset.
 *
 * @param name the name of the tree automaton
 * @param cset the set of stack symbols to check for conflicts.
 * @author Benedikt Nordhoff
 */
class SingleSetConflictTA[StackSymbol <% HasTermRepresentation](val name: String, cset: scala.collection.Set[StackSymbol]) extends IterableTreeAutomata {
  val isForwardRule = Set("cut", "final", "ret", "base", "call1", "acq")
  def genScript: String = {
    val buffer = new StringBuilder();
    import buffer.{ append => out }

    out("%Defining bad stack symbols\n")
    for (s <- cset) {
      out(name)
      out("_badStack(")
      out(s.toTerm)
      out(").\n")
    }

    out(
      """%defining conflict automata.
name_nil(X,1) :- name_badStack(X).
name_nil(X,0) :- not(name_badStack(X)).

name_ret(_,0).
name_base(_,X,X).
name_cut(_,X,X).
name_call1(_,X,X).

name_call2(_,1,0,1).
name_call2(_,0,1,1).
name_call2(_,0,0,0).
name_call2(_,1,1,2).
name_call2(_,_,2,2).
name_call2(_,2,_,2).

name_use(_,1,0,1).
name_use(_,0,1,1).
name_use(_,0,0,0).
name_use(_,1,1,2).
name_use(_,_,2,2).
name_use(_,2,_,2).

name_acq(_,X,X).
name_spawn(_,1,0,1).
name_spawn(_,0,1,1).
name_spawn(_,0,0,0).
name_spawn(_,1,1,2).
name_spawn(_,_,2,2).
name_spawn(_,2,_,2).
name_final(2).
""".replace("name", name))

    buffer.toString

  }

}
