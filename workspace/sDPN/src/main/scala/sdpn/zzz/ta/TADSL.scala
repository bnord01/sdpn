package sdpn.zzz.ta

import scala.collection.immutable.Queue


sealed trait Atom
case class ScalVar(name: String) extends Atom
abstract case class SetVar(name: String) extends Atom {
    def constraints: Queue[Constraint]    
}
case class SimpleSetVar(override val name: String) extends SetVar(name) {
    def constraints: Queue[Constraint] = Queue.empty
}
case class IntConst(value: Int) extends Atom

case class Union(a: SetVar, b: SetVar) extends SetVar(a.name + "_CUP_" + b.name) {
    val constraints = a.constraints ++ b.constraints + IsUnion(this, a, b)
}


case class SetVarPlus(a: SetVar, num: Int) extends SetVar(a.name) {    
    def constraints: Queue[Constraint] = a.constraints
}

sealed trait Constraint
case class Disjoint(a: SetVar, b: SetVar) extends Constraint
case class Contains(a: SetVar, num: Int) extends Constraint
case class IsUnion(ab: SetVar, a: SetVar, b: SetVar) extends Constraint
case class Predicate(name: String, args: Atom*) extends Constraint

object SetIntScalImplicits {
	

    //implicit def sym2SetVar[T](s: Symbol)(implicit o:SetVar => T):T = SimpleSetVar(s.name)
	implicit def sym2SetVar(s: Symbol) = SimpleSetVar(s.name)
    implicit def sym2Var(s: Symbol) = ScalVar(s.name)
    implicit def int2IntConst(i: Int) = IntConst(i)
    implicit def situp2SetVarPlus(p:(Symbol,Int)) = SetVarPlus(SimpleSetVar(p._1.name),p._2)
}

/**
 * Attempt to define a datalog like domain specific language to define tree automata.
 * The rules ended by ! are handled by the RuleManager defined by rm.
 * 
 * Abandoned because XSBs support for nested terms made it very easy to define 
 * tree automata directly. 
 * 
 * Right now when mixing in this trait you can write stuff like: 
 *  {{{
 * 'myrel('X, 'Y) :- 'otherRel('Y, 'X, '_) & 'anyRel('X)!
 *
 * 'blub('X, 'X)!
 *
 * 'keks(3, 1, '_)!
 *
 *  'x!
 *
 *  'x(3)!
 *
 *  'x('X, 2, '_) :- 'blub('X, 1, '_)!
 * }}}
 * @deprecated
 * @author Benedikt Nordhoff
 */
trait TADSL {
    type State <: Product
    /**
     * The RuleManager which is used to handle the rules defined by !.
     */
    def rm: RuleManager

    case class TARLhs(name: String, rank: Int, srcStates: Queue[State]) {
        def :->(s: State) = {
            assert(srcStates.length == rank)
            TARule(this, s, Queue.empty)
        }
        def apply(state: State) = {
            assert(srcStates.length < rank)
            TARLhs(name, rank, srcStates + state)
        }
    }

    case class TARule(lhs: TARLhs, targetState: State, consts: Queue[Constraint]) {
        def |(const: Constraint): TARule = {
            assert(consts.isEmpty)
            addConstraint(const)
        }
        def &(const: Constraint) = {
            assert(!consts.isEmpty)
            addConstraint(const)
        }
        def ! { rm.addTARule(this) }
        def addConstraint(const: Constraint) = TARule(lhs, targetState, (consts + const))

    }

}

trait RuleManager {
    def addTARule(rule: TADSL#TARule): Unit
}

/**
 * A RuleManager translating rules to Prolog.
 * @deprecated
 * @author Benedikt Nordhoff
 */
trait PrologTranslator extends RuleManager {

    def prefix: String
    def arityMap: Map[Int, Int]

    val symbolArityMap = scala.collection.mutable.Map[String, Int]()

    protected val buffer = new StringBuffer()
    import buffer.{ append => out }

    def addTARule(rule: TADSL#TARule) {

        var consts = printLHS(rule.lhs)
        out("  ,  ")
        consts ++= printState(rule.targetState)
        out(")")

        consts ++= rule.consts
        if (!consts.isEmpty) {
            out(" : - ")
            out(consts.mkString(","))
        }
        out(".\n")

    }
    protected def printLHS(lhs: TADSL#TARLhs): Queue[Constraint] = {
        var consts = Queue.empty[Constraint]
        out(prefix)
        out("_")
        out(lhs.name + "(")

        val it: Iterator[TADSL#State] = lhs.srcStates.iterator
        while (it.hasNext) {
            consts ++= printState(it.next)
            if (it.hasNext)
                out(",  ")
        }

        return consts

    }
    protected def printState(s: TADSL#State): Queue[Constraint] = {
        var consts = Queue.empty[Constraint]
        for (k <- 0 until s.productArity) {
            val a = s.productElement(k)
            if (a.isInstanceOf[Atom]) {
                a.asInstanceOf[Atom] match {
                    case ScalVar(p) => out(p)
                    case IntConst(i) => out(i)
                    //TODO need to manage nested SetVarPlus
                    case svp: SetVarPlus => {
                        val arity = arityMap(k)
                        val num = svp.num
                        assert(num <= arity,"arity smaller than num for set " + svp)
                        symbolArityMap.get(svp.name) match {
                            case Some(a) => assert(a == arity, "symbol with different arity")
                            case None => symbolArityMap += svp.name -> arity
                        }

                        for (i <- 1 until arity) {
                            if (i != num)
                                out(svp.name + "_" + i + ",")
                            else out("1,")
                        }
                        if (arity != num)
                            out(svp.name + "_" + arity + ",")
                        else out("1,")

                        consts ++= svp.constraints
                    }
                    case sv: SetVar => {
                        val arity = arityMap(k)
                        symbolArityMap.get(sv.name) match {
                            case Some(a) => assert(a == arity, "symbol with different arity")
                            case None => symbolArityMap += sv.name -> arity
                        }
                        for (i <- 1 until arity) {
                            out(sv.name + "_" + i + ",")
                        }
                        out(sv.name + "_" + arity)
                        consts ++= sv.constraints
                    }
                    
                }
            } else {
                throw new IllegalArgumentException("State contained non Atom element")
            }
            if (k < s.productArity - 1)
                out(", ")
        }

        return consts
    }

    def getRules = buffer.toString
}

object testi extends TADSL with PrologTranslator {
    import SetIntScalImplicits._
    
    
    override type State = (SetVar, ScalVar)
    override def rm = this
    override def prefix = "bb"
    override def arityMap = Map(0 -> 2)

    def CALL(s1: State)(s2: State) = TARLhs("Call", 2, Queue(s1, s2))


    
    def main(args: Array[String]) {
        CALL('X, 'x)('Y, 'y) :-> (Union('X, 'Y), 'z) | Disjoint('X, 'Y)!;

        println(rm.getRules)
        

    }
}