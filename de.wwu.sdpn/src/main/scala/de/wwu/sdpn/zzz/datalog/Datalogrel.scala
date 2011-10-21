package de.wwu.sdpn.zzz.datalog

/**
 * DSL for datalog like expresions.
 * @deprecated
 * @author Benedikt Nordhoff
 */
trait DatalogTerm 

case class Variable(name:Symbol) extends DatalogTerm{
	
	assert(name toString() substring(1, 2) matches "[A-Z_]")
	
	override def toString = name.toString.substring(1)
}
case class IntConst(value:Int) extends DatalogTerm {
	override def toString = value.toString
}
case class Atom(name:Symbol) extends DatalogTerm {
	assert(name toString() substring(1, 2) matches "[a-z]")	
	override def toString = name.toString.substring(1)
}

case class :- (rel:Datalogrel, conds:Datalogrel*){	
	override def toString = {
		rel + (if(conds.isEmpty) "." else " :- " + conds.mkString(", ") + ".")
	}
	def ! (implicit manager:DatalogManager) {
		manager.addCond(this)
	}	
	
	def & (nrel:Datalogrel) = new :- (rel,(conds++Array(nrel)):_*)
}

trait DatalogManager {
	def addCond(x : :-)
	def addRel(x:Datalogrel)
}


class Datalogrel(name:Atom, vars : DatalogTerm*) {
	
	def :- (rel:Datalogrel* ) : :- 	= { new :- (this,rel:_*) }
	
	def apply(nvars:DatalogTerm*) : Datalogrel= {
		new Datalogrel(name,(vars ++ nvars) :_*)
	}
	
	override def toString = name + "(" + vars.mkString(", ") + ")"
	
	def ! (implicit manager: DatalogManager) {
		manager.addRel(this)
	}	
}

object ?: {
	def apply(rel:Datalogrel) = rel
}

object Datalogrel {
	implicit def toRel (name:Symbol) = new Datalogrel(Atom(name))
	implicit def intToIntConst (x:Int) = IntConst(x)
	implicit def symbToVariable (x:Symbol) = Variable(x)
}