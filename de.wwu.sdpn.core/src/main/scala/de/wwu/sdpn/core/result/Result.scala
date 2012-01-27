package de.wwu.sdpn.core.result

sealed trait Result[K,KS,R] extends Mutable {
    def value: ResultValue
    def key: K
    def subResults:Map[KS,R]
    def hasSubResults:Boolean
    def updateValue(path: List[_], newValue: ResultValue): Unit
    def lookUp(path: List[_]): Result[_,_,_]
    override def toString() : String = Result.prettyPrint(this)
}

case class BaseResult[K](key:K, var value: ResultValue) extends Result[K,Nothing,Nothing] {
    def updateValue(path: List[_], newValue: ResultValue) {
        require(path == Nil, "Can't update result at path: " + path + " in BaseResult.")
        require(value == Undecidet, "Can't update non 'Undecidet' result! Old value: " + value + ", new value: " + newValue)
        value = newValue
    }
    def lookUp(path: List[_]) = {
        require(path == Nil, "Can't lookup result at path: " + path + " in BaseResult.")
        this
    }
    def hasSubResults = false
    def subResults = Map()
    
}

abstract case class ComposedResult[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](key: K, subResults: Map[SK, SR]) extends Result[K,SK,SR] {
    def updateValue(path: List[_], newValue: ResultValue) {
        path match {
            case (car:SK) :: cdr => subResults(car).updateValue(cdr, newValue)
            case Nil        => throw new IllegalArgumentException("Can't update composed result with Nil path.")
        }
    }
    def lookUp(path: List[_]) : Result[_,_,_]= {
        path match {
            case (car:SK) :: cdr => subResults(car).lookUp(cdr)
            case Nil        => this
        }
    }
    def hasSubResults = !subResults.isEmpty
}

class ConjunctiveResult[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](override val key: K, override val subResults: Map[SK,SR]) extends ComposedResult[K,SK,SR,SSK,SSR](key,subResults) {
    def value: ResultValue = {
        val valset: Set[ResultValue] = Set() ++ (subResults.values.map(_.value))
        if (valset contains Unprocessed)
            Unprocessed
        else if (valset contains Negative)
            Negative
        else if (valset contains ProcessingError)
            ProcessingError
        else if (valset contains Undecidet)
            Undecidet
        else
            Positive
    }
} 
object ConjunctiveResult {
    def apply[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](key:K,subResults:Map[SK,SR]) :  ConjunctiveResult[K,SK,SR,SSK,SSR]= new ConjunctiveResult[K,SK,SR,SSK,SSR](key,subResults)
    def unapply[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](cr : ConjunctiveResult[K,SK,SR,SSK,SSR]): Option[(K,Map[SK,SR])] = {
        Some(cr.key,cr.subResults)
    }
}

class DisjunctiveResult[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](override val key: K, override val subResults: Map[SK,SR]) extends ComposedResult[K,SK,SR,SSK,SSR](key,subResults) {
    def value: ResultValue = {
        val valset: Set[ResultValue] = Set() ++ (subResults.values.map(_.value))
        if (valset contains Unprocessed)
            Unprocessed
        else if (valset contains Positive)
            Positive
        else if (valset contains ProcessingError)
            ProcessingError
        else if (valset contains Undecidet)
            Undecidet
        else
            Negative
    }
}
object DisjunctiveResult {
    def apply[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](key:K,subResults:Map[SK,SR]) :  DisjunctiveResult[K,SK,SR,SSK,SSR]= new DisjunctiveResult[K,SK,SR,SSK,SSR](key,subResults)
    def unapply[K,SK,SR<:Result[SK,SSK,SSR],SSK,SSR](cr : DisjunctiveResult[K,SK,SR,SSK,SSR]): Option[(K,Map[SK,SR])] = {
        Some(cr.key,cr.subResults)
    }
}

class DisjunctiveRootResult[SK,SR<:Result[SK,SSK,SSR],SSK,SSR](override val subResults: Map[SK, SR]) extends DisjunctiveResult[Unit,SK,SR,SSK,SSR]((),subResults)
object DisjunctiveRootResult {
    def apply[SK,SR<:Result[SK,SSK,SSR],SSK,SSR](subResults:Map[SK,SR]) :  DisjunctiveRootResult[SK,SR,SSK,SSR]= new DisjunctiveRootResult[SK,SR,SSK,SSR](subResults)
    def unapply[SK,SR<:Result[SK,SSK,SSR],SSK,SSR](cr : DisjunctiveRootResult[SK,SR,SSK,SSR]): Option[Map[SK,SR]] = {
        Some(cr.subResults)
    }
}

class ConjunctiveRootResult[SK,SR<:Result[SK,SSK,SSR],SSK,SSR](override val subResults: Map[SK, SR]) extends ConjunctiveResult[Unit,SK,SR,SSK,SSR]((),subResults)
object ConjunctiveRootResult {
    def apply[SK,SR<:Result[SK,SSK,SSR],SSK,SSR](subResults:Map[SK,SR]) :  ConjunctiveRootResult[SK,SR,SSK,SSR]= new ConjunctiveRootResult[SK,SR,SSK,SSR](subResults)
    def unapply[SK,SR<:Result[SK,SSK,SSR],SSK,SSR](cr : ConjunctiveRootResult[SK,SR,SSK,SSR]): Option[Map[SK,SR]] = {
        Some(cr.subResults)
    }
}

sealed trait ResultValue 
sealed trait NormalResult extends ResultValue
sealed trait DefinitiveResult extends NormalResult

case object Positive extends DefinitiveResult
case object Negative extends DefinitiveResult
case object Undecidet extends NormalResult
case object Unprocessed extends ResultValue
case object ProcessingError extends ResultValue

object Result {
    def prettyPrint(res: Result[_,_,_], intention:String=""): String = {
        res match {
            case BaseResult(key,value) => intention + key + ": " + value.toString + "\n"
            case v:ComposedResult[_,_,_,_,_] => {            	
                val buf = new StringBuilder()
                buf append intention
                buf append v.key
                buf append ": "
                buf append v.value
                buf append "\n"
                for ((k,sr) <- v.subResults)
                    buf append (prettyPrint(sr,intention + "  "))
                buf toString
            }
        }
    }
}

