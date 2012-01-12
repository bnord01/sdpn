package de.wwu.sdpn.core.ta.xsb.reachability
import scala.collection.Set
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

class SingleSetReachingTA[StackSymbol <% HasTermRepresentation](override val name: String, reachingStack: Set[StackSymbol]) extends LockTreeAutomataAlphabet {
  val isForwardRule: Set[String] = Set[String]()
  val stateSize = 1

  def genScript(): String = {
    val buf = new StringBuilder()
    import buf.{ append => out }

    def outln(s: String) { out(s); out("\n") }

    for (s <- reachingStack)
      outln(name + "_rs(" + s.toTerm + ").")

    out("""
%Helper functions 
name_or(bot,bot,bot).
name_or(bot,top,top).
name_or(top,bot,top).
name_or(top,top,top).

%TA rules
name_ret(bot).
name_nil(t(_,S),top) :- name_rs(S).
name_nil(t(_,S),bot) :- not(name_rs(S)).

name_base(X,X).
name_call1(X,X).
name_acq(_,X,X).

name_use(_,X,Y,XoY) :- name_or(X,Y,XoY).
name_call2(X,Y,XoY) :- name_or(X,Y,XoY).
name_spawn(X,Y,XoY) :- name_or(X,Y,XoY).

name_final(top). 

""".replace("name", name))

    buf.toString
  }

}