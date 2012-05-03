package de.wwu.sdpn.core.ta.xsb

class StdLibLockOperations(val name: String = "sllo") extends LockOperations {  

  override def genScript: String = {
   """
%%% LockOperations based on the ugraphs and ordsets library from XSB
    
:- import 
        ord_add_element/3,
        ord_member/2,
        ord_union/3,
        ord_disjoint/2
        from ordsets.
:- import 
        graph_union/3,
        edges/2,
        add_edges/3
        from ugraphs.
:- import member/2 from basics.

%Operations on Lock Sets
name_emptySet([]).
name_isElemUnion(Elem,X,EX) :- ord_add_element(X,Elem,EX).
name_isElemUnion(Elem,X,Y,EXY) :- ord_add_element(X,Elem,EX), ord_union(EX,Y,EXY).
name_disjoint(X,Y) :- ord_disjoint(X,Y).
name_isNoElem(Elem,X) :- not(ord_member(Elem,X)).
name_isElem(Elem,X) :- ord_member(Elem,X).
name_isUnion(X,Y,XY) :- ord_union(X,Y,XY).
name_isUnion3(X,Y,Z,XYZ) :- ord_union(X,Y,XY),ord_union(XY,Z,XYZ).
name_isElemUnion2(Elem,X,Y,EXY) :- ord_add_element(X,Elem,EX), ord_union(EX,Y,EXY).

%Operations on Graphs
name_emptyGraph([]).
name_isGraphUnion(X,Y,XY) :- graph_union(X,Y,XY).
name_isGraphUnion3(X,Y,Z,XYZ) :- graph_union(X,Y,XY),graph_union(XY,Z,XYZ).
name_allEdges(_X,[],[]).
name_allEdges(X,[H|T],[X-H|XT]) :- name_allEdges(X,T,XT).

name_isGraphXUUnion(X,U,G,GUX) :- name_allEdges(X,U,XU), add_edges(G,XU,GUX).
name_isGraphXUUnion2(X,U,G1,G2,GUX) :- name_allEdges(X,U,XU), add_edges(G1,XU,G1UX), graph_union(G2,G1UX,GUX).      

        
:-table(name_edge/3).
name_edge(G,X,Y) :- edges(G,Edges), member(X-Y,Edges).

:-table(name_path/3).
name_path(G,X,Y) :- name_edge(G,X,Y).
name_path(G,X,Z) :- name_path(G,X,Y),name_edge(G,Y,Z).

name_notCyclic(G) :- not(name_path(G,X,X)).        
""".replace("name", name)
  }

}
