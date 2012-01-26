package de.wwu.sdpn.wala.analyses.datarace

sealed trait Result[K] extends Mutable {
    def value: ResultValue
    def updateValue(path: List[K], newValue: ResultValue): Unit
    def lookUp(path: List[K]): Result[K]
    override def toString() : String = Result.prettyPrint(this)
}

case class BaseResult[K](var value: ResultValue) extends Result[K] {
    def updateValue(path: List[K], newValue: ResultValue) {
        require(path == Nil, "Can't update result at path: " + path + " in BaseResult.")
        require(value == Undecidet, "Can't update non 'Undecidet' result! Old value: " + value + ", new value: " + newValue)
        value = newValue
    }
    def lookUp(path: List[K]) = {
        require(path == Nil, "Can't lookup result at path: " + path + " in BaseResult.")
        this
    }
}

abstract case class ComposedResult[K](subResults: Map[K, Result[K]]) extends Result[K] {
    def updateValue(path: List[K], newValue: ResultValue) {
        path match {
            case car :: cdr => subResults(car).updateValue(cdr, newValue)
            case Nil        => throw new IllegalArgumentException("Can't update composed result with Nil path.")
        }
    }
    def lookUp(path: List[K]) = {
        path match {
            case car :: cdr => subResults(car).lookUp(cdr)
            case Nil        => this
        }
    }
}

case class ConjunctiveResult[K](override val subResults: Map[K, Result[K]]) extends ComposedResult(subResults) {
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

case class DisjunctiveResult[K](override val subResults: Map[K, Result[K]]) extends ComposedResult(subResults) {
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

sealed trait ResultValue
sealed trait NormalResult extends ResultValue
sealed trait DefinitiveResult extends NormalResult

case object Positive extends DefinitiveResult
case object Negative extends DefinitiveResult
case object Undecidet extends NormalResult
case object Unprocessed extends ResultValue
case object ProcessingError extends ResultValue

object Result {
    def prettyPrint(res: Result[_], prefix: String = "Overall",intention:String=""): String = {
        res match {
            case BaseResult(value) => intention + prefix + ": " + value.toString + "\n"
            case v:ComposedResult[_] => {            	
                val buf = new StringBuilder()
                buf append intention
                buf append prefix
                buf append ": "
                buf append v.value
                buf append "\n"
                for ((k,sr) <- v.subResults)
                    buf append (prettyPrint(sr,k.toString(),intention + "  "))
                buf toString
            }
        }
    }
}

