package de.wwu.sdpn.core.ta.xsb.iterable

import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata

class CutTransducer (cutNumber:Int,other:ScriptTreeAutomata,name0:String=null) extends IterableTreeAutomata {
    require(other.alphabet == alphabet,"Can cut transduce only IterableTreeAutomata!")
    def name = if(name0 != null) name0 else "cut" + cutNumber + "_" + other.name
    require(!(other.boundNames contains name))
    override def boundNames = other.boundNames + name
    
    def isForwardRule = Set() //Set("nil","ret","base","call1","acq","final")
    def genScript: String = {
        val buf = new StringBuilder()
        import buf.{ append => out }
        def outln(s: String) { out(s); out("\n") }
     
        outln("""
                %%% The tree automata to be checkt at cut number: """ + cutNumber + "\n")
        outln(other.genScript)
        
        val c1 = "sdfbj8900uzu23hjsdf89hio34t79gdh98erzhnbcs98hj4235fdg0u9ij"
        val c2 = "4567079gbvb89sz9hjlnadf90s7hbwjnayd0v897z8hjn2309a7dsfzhnl"
        out(
"""
                %%% New rules for the iterated automata
name_nil(GS,b(Q)) :- other_nil(GS,Q).
name_nil(GS,r) :- not(other_nil(GS,_)).
                
name_ret(b(Q)) :- other_ret(Q).
name_ret(r) :- not(other_ret(_)).

name_call1(b(Q1),b(Q2)) :- other_call1(Q1,Q2).
name_call1(t(Q1),t(Q2)) :- other_call1(Q1,Q2).
name_call1(r,r).
name_call1(b(Q),r) :- not(other_call1(Q,_)).
                
name_base(A,b(Q1),b(Q2)) :- other_base(A,Q1,Q2).
name_base(A,t(Q1),t(Q2)) :- other_base(A,Q1,Q2).
name_base(_,r,r).
name_base(A,b(Q),r) :- not(other_base(A,Q,_)).

name_acq(A,b(Q1),b(Q2)) :- other_acq(A,Q1,Q2).
name_acq(A,t(Q1),t(Q2)) :- other_acq(A,Q1,Q2).
name_acq(_,r,r).
name_acq(A,b(Q),r) :- not(other_acq(A,Q,_)).

                %%% A returning call %%%
                
% Some subtree couldn't be accepted by the other automata continue propagating 'r'                
name_call2(r,r,r).
name_call2(b(_),r,r).
name_call2(r,b(_),r).                

% If the other ta can continque let him.                
name_call2(b(Qc),b(Qr),b(Q)) :- other_call2(Qc,Qr,Q).
name_call2(b(Qc),t(Qr),t(Q)) :- other_call2(Qc,Qr,Q).                

% Cut in the returning branch, handle as non returning call.                
name_call2(t(Qr),_,t(Q)) :- other_call1(Qr,Q).

% No rule to handle, go to state r                 
name_call2(b(Qc),b(Qr),r) :- not(other_call2(Qc,Qr,_)).                

                %%% A returning use just like %%%
                
% Some subtree couldn't be accepted by the other automata continue propagating 'r'                
name_use(_,r,r,r).
name_use(_,b(_),r,r).
name_use(_,r,b(_),r).                

% If the other ta can continque let him.                
name_use(A,b(Qc),b(Qr),b(Q)) :- other_use(A,Qc,Qr,Q).
name_use(A,b(Qc),t(Qr),t(Q)) :- other_use(A,Qc,Qr,Q).                

% Cut in the returning branch, handle as non returning call.                
name_use(A,t(Qr),_,t(Q)) :- other_acq(A,Qr,Q).

% No rule to handle, go to state r
name_use(A,b(Qc),b(Qr),r) :- not(other_use(A,Qc,Qr,_)).                

                %%% A spawn %%%
                
% Other ta can handle the cases, let him.                 
name_spawn(b(Qs),b(Qr),b(Q)) :- other_spawn(Qs,Qr,Q).                
name_spawn(b(Qs),t(Qr),t(Q)) :- other_spawn(Qs,Qr,Q).
name_spawn(t(Qs),b(Qr),b(Q)) :- other_spawn(Qs,Qr,Q).                
name_spawn(t(Qs),t(Qr),t(Q)) :- other_spawn(Qs,Qr,Q).                
                
% Some subtree was already rejected                
name_spawn(b(_),r,r).
name_spawn(r,_,r).
% If the spawned thread has a cut, there is no reason to continue!
                
% Other ta can not handle, but we could be under the cut, go to r.                
name_spawn(b(Qs),b(Qr),r) :- not(other_spawn(Qs,Qr,_)).               
               
                %%% The cut we are looking for %%%
name_cut(cpt(cutNumber,S),b(_),t(Q)) :- other_nil(S,Q).                
name_cut(cpt(cutNumber,S),r,t(Q)) :- other_nil(S,Q).

                %%% The cut's we don't care about %%%
""".replace("cutNumber",cutNumber.toString).replace("name",c1).replace("other",c2).replace(c1,name).replace(c2,other.name)
        )
        
        for(i <- 0 until cutNumber){
            out(""" 
name_cut(cpt(cutNumber,S),b(Q1),b(Q2)) :- other_cut(cpt(cutNumber,S),Q1,Q2).                
name_cut(cpt(cutNumber,S),t(Q1),t(Q2)) :- other_cut(cpt(cutNumber,S),Q1,Q2).  
""".replace("cutNumber",i.toString).replace("name",c1).replace("other",c2).replace(c1,name).replace(c2,other.name))
        }
        
        out(""" 
                %%% Final states %%%
                
name_final(t(Q)) :- other_final(Q).                
                """.replace("name",c1).replace("other",c2).replace(c1,name).replace(c2,other.name)
                
        )
        
        
        return buf.toString
    }

}