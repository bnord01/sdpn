package de.wwu.sdpn.core.textual
import scala.util.parsing.combinator.JavaTokenParsers
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation
import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.PopRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.DPNRule
import de.wwu.sdpn.core.dpn.monitor.DPNUtil
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.gui.MonitorDPNView
import de.wwu.sdpn.core.analyses.TSRTask
import java.io.File
import java.io.Reader
import java.io.BufferedReader
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
/**
 * We use the following grammar:
 * term           ::= [1-9][0-9]* | [a-z][a-zA-Z0-9_]*
 * name           ::= STRINGLITERAL "..."
 * named          ::= term ":" name
 * repnamed       ::= [named { [","] named}]
 * rule           ::= base_rule | pop_rule | simple_push | lock_push | spawn_rule
 * controls       ::= "controls" "{" repnamed "}"
 * stacks         ::= "stacks" "{" repnamed "}"
 * locks          ::= "locks" "{" repnamed "}"
 * actions        ::= "actions" "{" repnamed "}"
 * naming         ::= (controls | stacks | locks | actions)*
 *
 * control        ::= term
 * stack          ::= term
 * action 	      ::= term
 * lock           ::= term
 * base_rule      ::= "(" control "," stack ")" "--" action "-->" "(" control "," stack ")"
 * pop_rule       ::= "(" control "," stack ")" "--" action "-->" "(" control ")"
 * spawn_rule     ::= "(" control "," stack ")" "--" action "-->" "(" control "," stack ":>" control "," stack ")"
 * simple_push    ::= "(" control "," stack ")" "--" action "-->" "(" control "," stack "," stack ")"
 * lock_push      ::= "(" control "," stack ")" "--" action "-" lock "-->" "(" control "," stack "," stack ")"
 *
 * rules          ::= "rules" "{" [rule { "," rule }] "}"
 * initial_conf   ::= "initial" "(" control "," stack ")"
 *
 * dpn            ::= "dpn" "{" naming initial_conf rules "}"
 *
 * repstack       ::= stack { [","] stack}
 * tsr_conflict   ::= ["check"] "conflict" ["nolocks"] ["between"] "{" repstack "}" ["and"] "{” repstack "}"
 *
 * tsr_task       ::= dpn tsr_conflict_def
 *
 *
 */
object DPNParser extends JavaTokenParsers {

    def main(args: Array[String]): Unit = {
        var explore = false
        var file: String = ""
        if (args.length == 2) {
            explore = (args(0) == "-e" || args(0) == "--explore")
            file = args(1)
        } else if (args.length == 1) {
            file = args(0)
        } else {
            println("Please supply the name of the file containing the task definition as single argument!")
            System.exit(-1)
        }
        val f = new File(file)
        require(f.exists() && f.canRead(), "Couldn't read file: " + args(0))
        val rdr = new BufferedReader(new FileReader(f))
        if (explore) {
            val pres = parseDPN(rdr)
            if (pres.successful) {
                val dpn = pres.get._1
                MonitorDPNView.show(dpn,true)
            } else {
                println("Parsing error!")
                println(pres)
                System.exit(-1)
            }

        } else {
            val pres = parseTSRTask(rdr)
            if (pres.successful) {
                val result = pres.get.run
                println(result)
                if (result) {
                    System.err.println("A conflict could exist.")
                    System.exit(0)
                } else {
                    System.err.println("No conflict can exist.")
                    System.exit(0)
                }
            } else {
                println("Parsing error!")
                println(pres)
                System.exit(-1)
            }
        }
    }
    
    // format: OFF
    def term: Parser[String] = "[1-9][0-9]*".r | "[a-z][a-zA-Z0-9_]*".r
    def name: Parser[String] = stringLiteral ^^(x => x.substring(1,x.length - 1))
    def controlDef: Parser[(String, Control)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> Control(term, name) }
    def stackDef: Parser[(String, Stack)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> Stack(term, name) }
    def actionDef: Parser[(String, Action)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> Action(term, name) }
    def lockDef: Parser[(String, Lock)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> Lock(term, name) }
    def controls : Parser[Naming] = 
        "controls"~>"{"~>repsep(controlDef,opt(","))<~"}" ^^ {x => defaultNaming.copy(cname=(cdef ++ x))}
    def stacks : Parser[Naming] = 
        "stacks"~>"{"~>repsep(stackDef,opt(","))<~"}" ^^ {x => defaultNaming.copy(sname=(sdef ++ x))}
    def actions : Parser[Naming] = 
        "actions"~>"{"~>repsep(actionDef,opt(","))<~"}" ^^ {x => defaultNaming.copy(aname=(adef ++ x))}
    def locks : Parser[Naming] = 
        "locks"~>"{"~>repsep(lockDef,opt(","))<~"}" ^^ {x => defaultNaming.copy(lname=ldef ++ x)}
    def naming : Parser[Naming]  = ((controls | stacks | actions | locks)*)^^{_.fold(defaultNaming)((x,y) => x + y)}
    
    def control(implicit n: Naming) : Parser[Control] = term ^^ {n cname _}
    def stack(implicit n: Naming) : Parser[Stack] = term ^^ {n sname _}
    def lock(implicit n: Naming) : Parser[Lock] = term ^^ {n lname _}
    def action(implicit n: Naming) : Parser[Action] = term ^^ {n aname _}
    
    def initial(implicit n:Naming) : Parser[(Control,Stack)] = "initial"~>"("~control~","~stack~")"^^{case "("~control~","~stack~")" => (control,stack)}
    
    def  base_rule(implicit n:Naming)      : Parser[BaseRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~c1~","~s1~")" => BaseRule(c,s,a,c1,s1)
    }
    def  pop_rule(implicit n:Naming)      : Parser[PopRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~c1~")" => PopRule(c,s,a,c1)
    }
    def  spawn_rule(implicit n:Naming)     : Parser[SpawnRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~","~stack~":>"~control~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~cr~","~sr~":>"~cs~","~ss~")" => SpawnRule(c,s,a,cr,sr,cs,ss)
    }
    def  simple_push(implicit n:Naming)     : Parser[PushRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~","~stack~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~cr~","~ss~","~sr~")" => PushRule(c,s,a,cr,ss,sr)
    }
    def  lock_push(implicit n:Naming)     : Parser[PushRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-"~lock~"-->"~"("~control~","~stack~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-"~l~"-->"~"("~cr~","~ss~","~sr~")" => PushRule(c,s,LockAction(a,l),cr,ss,sr)
    }
    def rule(implicit n:Naming) : Parser[DPNRule[Control,Stack,Action]] = base_rule | pop_rule | spawn_rule | simple_push | lock_push
    
    def rules(implicit n: Naming) : Parser[Set[DPNRule[Control,Stack,Action]]] = "rules"~"{"~>repsep(rule,opt(","))<~"}" ^^ {Set() ++ _}
    
    def dpn :Parser [(MonitorDPN[Control,Stack,Action,Lock],Naming)] =
        "dpn"~"{"~>naming into {implicit n => (initial ~ rules ^^ {case i~r => (DPNUtil.createMDPNfromRules(i,r,lockMap),n)}) <~ "}"}
        
        
    def repstack(implicit n:Naming) :Parser[Set[Stack]] = rep1sep(stack,opt(",")) ^^ {Set() ++ _}
    
    def tsr_conflict(implicit n:Naming) : Parser[(Set[Stack],Set[Stack],Boolean)] = 
        opt("check")~>"conflict"~>opt("nolocks")~(opt("between")~>"{"~>repstack~("}"~>opt("and")~>"{"~>repstack<~"}")) ^^ {
            case Some(_)~(c1~c2) => (c1,c2,false)
            case None~(c1~c2) => (c1,c2,true)
        }
        
    def tsr_task: Parser[TSRTask[Control,Stack,Action,Lock]] = 
        dpn into {x =>  tsr_conflict(x._2) ^^ {case (c1,c2,lockSens) => TSRTask(x._1,c1,c2,lockSens)}}

    // format: ON
    def parseDPN(str: String): ParseResult[(MonitorDPN[Control, Stack, Action, Lock], Naming)] = parse(dpn, str)
    def parseDPN(rdr: Reader): ParseResult[(MonitorDPN[Control, Stack, Action, Lock], Naming)] = parse(dpn, rdr)

    def parseTSRTask(str: String): ParseResult[TSRTask[Control, Stack, Action, Lock]] = parse(tsr_task, str)
    def parseTSRTask(rdr: Reader): ParseResult[TSRTask[Control, Stack, Action, Lock]] = parse(tsr_task, rdr)
    val lockMap: (DPNRule[Control, Stack, Action] => Option[Lock]) =
        (r: DPNRule[Control, Stack, Action]) => r match {
            case PushRule(_, _, LockAction(_, l), _, _, _) => Some(l)
            case _                                         => None
        }

    val cdef = Map[String, Control]().withDefault(x => Control(x, x))
    val sdef = Map[String, Stack]().withDefault(x => Stack(x, x))
    val adef = Map[String, Action]().withDefault(x => Action(x, x))
    val ldef = Map[String, Lock]().withDefault(x => Lock(x, x))

    val defaultNaming = Naming(cdef, sdef, adef, ldef)

}

case class Naming(cname: Map[String, Control], sname: Map[String, Stack], aname: Map[String, Action], lname: Map[String, Lock]) {
    def laname(a: String, l: String) = LockAction(aname(a), lname(l))
    def +(other: Naming): Naming = {
        other match {
            case Naming(c, s, a, l) => Naming(cname ++ c, sname ++ s, aname ++ a, lname ++ l)
        }
    }
}

sealed abstract case class NamedTerm(term: String, name: String) extends HasTermRepresentation {
    def toTerm = term
    override def toString = name
}
case class Control(term0: String, name0: String) extends NamedTerm(term0, name0)
case class Stack(term0: String, name0: String) extends NamedTerm(term0, name0)
case class Action(term0: String, name0: String) extends NamedTerm(term0, name0)
case class Lock(term0: String, name0: String) extends NamedTerm(term0, name0)
case class LockAction(action: Action, lock: Lock) extends Action(action.term, action.name + "-" + lock.name)