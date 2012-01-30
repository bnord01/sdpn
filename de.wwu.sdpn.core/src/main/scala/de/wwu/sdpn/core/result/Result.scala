package de.wwu.sdpn.core.result

sealed trait Result[K,D,KS,R] extends Mutable {
    def value: ResultValue
    def key: K
    def detail: D
    def subResults:Map[KS,R]
    def hasSubResults:Boolean
    def updateValue(path: List[_], newValue: ResultValue): Unit
    def lookUp(path: List[_]): Result[_,_,_,_]
    override def toString() : String = Result.prettyPrint(this)
}

case class BaseResult[K,D](key:K, var detail: D, var value: ResultValue) extends Result[K,D,Nothing,Nothing] {
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

abstract case class ComposedResult[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](key: K, var detail:D, subResults: Map[SK, SR]) extends Result[K,D,SK,SR] {
    def updateValue(path: List[_], newValue: ResultValue) {
        path match {
            case (car:SK) :: cdr => subResults(car).updateValue(cdr, newValue)
            case Nil        => throw new IllegalArgumentException("Can't update composed result with Nil path.")
        }
    }
    def lookUp(path: List[_]) : Result[_,_,_,_]= {
        path match {
            case (car:SK) :: cdr => subResults(car).lookUp(cdr)
            case Nil        => this
        }
    }
    def hasSubResults = !subResults.isEmpty
}

class ConjunctiveResult[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](override val key: K, detail0 :D , override val subResults: Map[SK,SR]) extends ComposedResult[K,D,SK,SR,SD,SSK,SSR](key,detail0,subResults) {
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
    def apply[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](key:K,detail:D,subResults:Map[SK,SR]) :  ConjunctiveResult[K,D,SK,SR,SD,SSK,SSR]= new ConjunctiveResult[K,D,SK,SR,SD,SSK,SSR](key,detail,subResults)
    def unapply[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](cr : ConjunctiveResult[K,D,SK,SR,SD,SSK,SSR]): Option[(K,D,Map[SK,SR])] = {
        Some(cr.key,cr.detail,cr.subResults)
    }
}

class DisjunctiveResult[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](override val key: K, detail0 :D , override val subResults: Map[SK,SR]) extends ComposedResult[K,D,SK,SR,SD,SSK,SSR](key,detail0,subResults) {
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
    def apply[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](key:K,detail:D,subResults:Map[SK,SR]) :  DisjunctiveResult[K,D,SK,SR,SD,SSK,SSR]= new DisjunctiveResult[K,D,SK,SR,SD,SSK,SSR](key,detail,subResults)
    def unapply[K,D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](cr : DisjunctiveResult[K,D,SK,SR,SD,SSK,SSR]): Option[(K,D,Map[SK,SR])] = {
        Some(cr.key,cr.detail,cr.subResults)
    }
}

class DisjunctiveRootResult[D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](detail0: D, override val subResults: Map[SK, SR]) extends DisjunctiveResult[Unit,D,SK,SR,SD,SSK,SSR]((),detail0,subResults)
object DisjunctiveRootResult {
    def apply[D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](detail:D , subResults:Map[SK,SR]) :  DisjunctiveRootResult[D,SK,SR,SD,SSK,SSR]= new DisjunctiveRootResult[D,SK,SR,SD,SSK,SSR](detail,subResults)
    def unapply[D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](cr : DisjunctiveRootResult[D,SK,SR,SD,SSK,SSR]): Option[(D,Map[SK,SR])] = {
        Some(cr.detail,cr.subResults)
    }
}

class ConjunctiveRootResult[D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](detail0: D, override val subResults: Map[SK, SR]) extends ConjunctiveResult[Unit,D,SK,SR,SD,SSK,SSR]((),detail0,subResults)
object ConjunctiveRootResult {
    def apply[D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](detail:D , subResults:Map[SK,SR]) :  ConjunctiveRootResult[D,SK,SR,SD,SSK,SSR]= new ConjunctiveRootResult[D,SK,SR,SD,SSK,SSR](detail,subResults)
    def unapply[D,SK,SR<:Result[SK,SD,SSK,SSR],SD,SSK,SSR](cr : ConjunctiveRootResult[D,SK,SR,SD,SSK,SSR]): Option[(D,Map[SK,SR])] = {
        Some(cr.detail,cr.subResults)
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
    def prettyPrint(res: Result[_,_,_,_], intention:String=""): String = {
        res match {
            case BaseResult(key,_,value) => intention + key + ": " + value.toString + "\n"
            case v:ComposedResult[_,_,_,_,_,_,_] => {      
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

