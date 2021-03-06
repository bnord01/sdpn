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
 * This is a parser for a textual representation of a DPN and a reachability check.
 *
 * It can be used from the command line with the name of a file, containing the textual
 * representation, as an argument.
 * The program then prints `true` resp. `false` to stdout if
 * the conflict is reachable resp. not reachable.
 *
 * Moreover, if the option `--explore`  is given as the first argument ,
 * a viewer is started which simulates the DPN instead.
 *
 * To run the analysis the `sdpn.properties` file, which specifies the path to the xsb executable
 * and a directory where temporary files are stored, needs to be either present in the current directory,
 * on the class path or specified via -Dsdpn.properties <path to file>.
 *
 * The file given as argument should contain a tsr_task as defined by the following grammar.
 *
 * Names are optional and are only used when exploring the DPN.
 * In later versions they may be used to represent a generated witness.
 *
 * ==Grammar in EBNF==
 * The parser uses the following grammar which is in EBNF except for the rules for `term` and `name`.
 *
 * `term` is a natural number or a combination of lower and upper case alpha numerical
 * symbols and _ which starts with a lower case letter.
 *
 * `name` can be any string literal enclosed in `"` as accepted by java.
 *
 * When using the optional `nolocks` keyword in the task definition a lock insensitive analysis is performed.
 *
 * In all lists commas are optional.
 *
 * {{{
 * atom           ::= "0" | -?[1-9][0-9]* | [a-z][a-zA-Z0-9_]*
 * term           ::= (atom [tuple]) | tuple
 * tuple          ::= "(" [term {"," term}] ")"
 * name           ::= STRINGLITERAL "..."
 * named          ::= term ":" name
 * repnamed       ::= [named { [","] named}]
 *
 * controls       ::= "controls" "{" repnamed "}"
 * stacks         ::= "stacks" "{" repnamed "}"
 * locks          ::= "locks" "{" repnamed "}"
 * actions        ::= "actions" "{" repnamed "}"
 * naming         ::= (controls | stacks | locks | actions)*
 *
 * control        ::= term
 * stack          ::= term
 * action         ::= term
 * lock           ::= term
 * base_rule      ::= "(" control "," stack ")" "--" action "-->" "(" control "," stack ")"
 * pop_rule       ::= "(" control "," stack ")" "--" action "-->" "(" control ")"
 * spawn_rule     ::= "(" control "," stack ")" "--" action "-->" "(" control "," stack ":>" control "," stack ")"
 * simple_push    ::= "(" control "," stack ")" "--" action "-->" "(" control "," stack "," stack ")"
 * lock_push      ::= "(" control "," stack ")" "--" action "-" lock "-->" "(" control "," stack "," stack ")"
 *
 * s_base_rule      ::= "(" stack ")" "--" action "-->" "(" stack ")"
 * s_pop_rule       ::= "(" stack ")" "--" action "-->" "(" ")"
 * s_spawn_rule     ::= "(" stack ")" "--" action "-->" "(" stack ":>" stack ")"
 * s_simple_push    ::= "(" stack ")" "--" action "-->" "(" stack "," stack ")"
 * s_lock_push      ::= "(" stack ")" "--" action "-" lock "-->" "(" stack "," stack ")"
 *
 * rule           ::= base_rule | pop_rule | simple_push | lock_push | spawn_rule |
 *                    s_base_rule | s_pop_rule | s_simple_push | s_lock_push | s_spawn_rule
 * rules          ::= "rules" "{" [rule { [","] rule }] "}"
 * initial_conf   ::= "initial" "(" [control ","] stack ")"
 *
 * dpn            ::= "dpn" "{" naming initial_conf rules "}"
 *
 * repstack       ::= stack { [","] stack}
 * tsr_conflict   ::= ["check" ["for"]] "conflict" ["nolocks"] ["between"] "{" repstack "}" ["and"] "{" repstack "}"
 *
 * tsr_task       ::= dpn tsr_conflict_def
 * }}}
 *
 * ==Example==
 * The following is an example of a simple TSRTask which can be passed to the parser and prints the result `false`.
 * It uses numbers as terms.
 *
 * {{{
 * dpn{
 *    controls {1:"Normal"}
 *    stacks {
 *        1:"Start"
 *        2:"Before monitor enter"
 *        3:"Inside Monitor, Spawning (1,1)"
 *        4: "Just spawned, now returning to 10"
 *        10:"Monitor return point. Going to Start"
 *    }
 *    actions {
 *        1:"base"
 *        2:"Entering monitor"
 *        3:"Spawning"
 *        4:"Leaving monitor"
 *        5:"Resuming to start"
 *    }
 *    locks {
 *        1:"Lock number one"
 *    }
 *
 *    initial(1,1)
 *
 *    rules {
 *        (1,1) --1--> (1,2)
 *        (1,2) --2-1--> (1,3,10)
 *        (1,3) --3--> (1,4:>1,1)
 *        (1,4) --4--> (1)
 *        (1,10) --5--> (1,2)
 *    }
 * }
 *
 * conflict between {4} and {4}
 * }}}
 *
 * @author Benedikt Nordhoff
 *
 */
object DPNParser extends JavaTokenParsers {
    
    // format: OFF
    protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
    
    def atom: Parser[String] = "0" | "-?[1-9][0-9]*".r | "[a-z][a-zA-Z0-9_]*".r
    def term: Parser[String] = (atom ~ opt(tuple) ^^ {case a~None => a; case a~Some(t) => a + t}) | tuple
    def tuple: Parser[String] = "("~>repsep(term,",")<~")"^^{case ls => ls.mkString("(",",",")")}
    def name: Parser[String] = stringLiteral ^^(x => x.substring(1,x.length - 1))
    def controlDef: Parser[(String, Control)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> Control(term, name) }
    def stackDef: Parser[(String, Stack)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> Stack(term, name) }
    def actionDef: Parser[(String, BaseAction)] = term ~ ":" ~ name ^^ { case term ~ ":" ~ name => term -> BaseAction(term, name) }
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
    def action(implicit n: Naming) : Parser[BaseAction] = term ^^ {n aname _}
    
    def nostate(implicit n:Naming) : Control = n.cname("0")
    
    def initial(implicit n:Naming) : Parser[(Control,Stack)] = 
        "initial"~>"("~control~","~stack~")"^^{case "("~control~","~stack~")" => (control,stack)} |
        "initial"~>"("~stack~")"^^{case "("~stack~")" => (nostate,stack)}        
    
    def  base_rule(implicit n:Naming) : Parser[BaseRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~c1~","~s1~")" => BaseRule(c,s,a,c1,s1)
    } 
    
    def  pop_rule(implicit n:Naming) : Parser[PopRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~c1~")" => PopRule(c,s,a,c1)
    }
    def  spawn_rule(implicit n:Naming) : Parser[SpawnRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~","~stack~":>"~control~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~cr~","~sr~":>"~cs~","~ss~")" => SpawnRule(c,s,a,cr,sr,cs,ss)
    }
    def  simple_push(implicit n:Naming) : Parser[PushRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-->"~"("~control~","~stack~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-->"~"("~cr~","~ss~","~sr~")" => PushRule(c,s,a,cr,ss,sr)
    }
    def  lock_push(implicit n:Naming) : Parser[PushRule[Control,Stack,Action]] = "("~control~","~stack~")"~"--"~action~"-"~lock~"-->"~"("~control~","~stack~","~stack~")" ^^{
        case "("~c~","~s~")"~"--"~a~"-"~l~"-->"~"("~cr~","~ss~","~sr~")" => PushRule(c,s,LockAction(a,l),cr,ss,sr)
    }
    
    def  s_base_rule(implicit n:Naming) : Parser[BaseRule[Control,Stack,Action]] = "("~stack~")"~"--"~action~"-->"~"("~stack~")" ^^{
        case "("~s~")"~"--"~a~"-->"~"("~s1~")" => BaseRule(nostate,s,a,nostate,s1)
    } 
    
    def  s_pop_rule(implicit n:Naming) : Parser[PopRule[Control,Stack,Action]] = "("~stack~")"~"--"~action~"-->"~"("~")" ^^{
        case "("~s~")"~"--"~a~"-->"~"("~")" => PopRule(nostate,s,a,nostate)
    }
    def  s_spawn_rule(implicit n:Naming)     : Parser[SpawnRule[Control,Stack,Action]] = "("~stack~")"~"--"~action~"-->"~"("~stack~":>"~stack~")" ^^{
        case "("~s~")"~"--"~a~"-->"~"("~sr~":>"~ss~")" => SpawnRule(nostate,s,a,nostate,sr,nostate,ss)
    }
    def  s_simple_push(implicit n:Naming)     : Parser[PushRule[Control,Stack,Action]] = "("~stack~")"~"--"~action~"-->"~"("~stack~","~stack~")" ^^{
        case "("~s~")"~"--"~a~"-->"~"("~ss~","~sr~")" => PushRule(nostate,s,a,nostate,ss,sr)
    }
    def  s_lock_push(implicit n:Naming)     : Parser[PushRule[Control,Stack,Action]] = "("~stack~")"~"--"~action~"-"~lock~"-->"~"("~stack~","~stack~")" ^^{
        case "("~s~")"~"--"~a~"-"~l~"-->"~"("~ss~","~sr~")" => PushRule(nostate,s,LockAction(a,l),nostate,ss,sr)
    }
    
    def rule(implicit n:Naming) : Parser[DPNRule[Control,Stack,Action]] = 
        base_rule | pop_rule | spawn_rule | simple_push | lock_push | 
        s_base_rule | s_pop_rule | s_spawn_rule | s_simple_push | s_lock_push
    
    def rules(implicit n: Naming) : Parser[Set[DPNRule[Control,Stack,Action]]] = "rules"~"{"~>repsep(rule,opt(","))<~"}" ^^ {Set() ++ _}
    
    def dpn :Parser [(MonitorDPN[Control,Stack,Action,Lock],Naming)] =
        "dpn"~"{"~>naming into {implicit n => (initial ~ rules ^^ {case i~r => (DPNUtil.createMDPNfromRules(i,r,lockMap),n)}) <~ "}"}
        
        
    def repstack(implicit n:Naming) :Parser[Set[Stack]] = rep1sep(stack,opt(",")) ^^ {Set() ++ _}
    
    def tsr_conflict(implicit n:Naming) : Parser[(Set[Stack],Set[Stack],Boolean)] = 
        opt("check"~opt("for"))~>"conflict"~>opt("nolocks")~(opt("between")~>"{"~>repstack~("}"~>opt("and")~>"{"~>repstack<~"}")) ^^ {
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
    val adef = Map[String, BaseAction]().withDefault(x => BaseAction(x, x))
    val ldef = Map[String, Lock]().withDefault(x => Lock(x, x))

    val defaultNaming = Naming(cdef, sdef, adef, ldef)

    
    /**
     * Main method to run this from the command line. 
     * options -e, -w, -s
     */
    def main(args: Array[String]): Unit = {
        def printUseage() {
            println("""usage: command [-ewsh] <file> [-ewsh]
                    
    where <file> is a file containing a well formed TSRTask 
    -e or --explore indicates that the dpn should be simulated (explored) 
    -w or --witness indicates that a witness should be printed if the dpn isn't simulated
    -s or --states  indicates that a witness annotated with the states of the used tree automata should be printed
    -h or --help    indicates that this help should be printed. Nothig else is done.""")
        }

        val estr = Set("-e", "--explore")
        val wstr = Set("-w", "--witness")
        val sstr = Set("-s", "--states")
        val hstr = Set("-h", "--help")
        val ops = estr union wstr union sstr union hstr

        val explore = args.exists(estr)
        val witness = args.exists(wstr union sstr)
        val states = args.exists(sstr)
        val help = args.exists(hstr)
        
        if(help) {
            printUseage()
            System.exit(0)
        }
        
        val noOps = args.filter(x => !(ops(x)))
        
        if (noOps.length != 1) {
            printUseage()
            System.exit(0)
        }
        val file = noOps(0)

        val f = new File(file)
        require(f.exists() && f.canRead(), "Couldn't read file: " + args(0))
        val rdr = new BufferedReader(new FileReader(f))
        if (explore) {
            val pres = parseDPN(rdr)
            if (pres.successful) {
                val dpn = pres.get._1
                MonitorDPNView.show(dpn, true)
            } else {
                println("Parsing error!")
                println(pres)
                System.exit(-1)
            }

        } else {
            val pres = parseTSRTask(rdr)
            if (pres.successful) {
                if (!witness) {
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
                    val result = pres.get.runWitness
                    if (result.isDefined) {
                        println("witness")
                        println(if(states) result.get.printTreeStates else result.get.printTree)
                        System.err.println("A conflict could exist.")
                        System.exit(0)
                    } else {
                        println("no witness")
                        System.err.println("No conflict can exist.")
                        System.exit(0)
                    }
                }
            } else {
                println("Parsing error!")
                println(pres)
                System.exit(-1)
            }
        }
    }
}

case class Naming(cname: Map[String, Control], sname: Map[String, Stack], aname: Map[String, BaseAction], lname: Map[String, Lock]) {
    def laname(a: String, l: String) = LockAction(aname(a), lname(l))
    def +(other: Naming): Naming = {
        other match {
            case Naming(c, s, a, l) => Naming(cname ++ c, sname ++ s, aname ++ a, lname ++ l)
        }
    }
}

sealed trait NamedTerm extends HasTermRepresentation {
    def term: String
    def name: String
    override def toTerm = term
    override def toString = name
}
case class Control(term: String, name: String) extends NamedTerm
case class Stack(term: String, name: String) extends NamedTerm
sealed trait Action extends NamedTerm
case class BaseAction(term: String, name: String) extends Action
case class Lock(term: String, name: String) extends NamedTerm
case class LockAction(action: BaseAction, lock: Lock) extends Action {
    val term = action.term
    val name = action.name
}
