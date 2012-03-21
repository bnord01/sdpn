package de.wwu.sdpn.core.result

sealed trait Result[D, KS, R] extends Mutable {
    def value: ResultValue
    def detail: D
    def subResults: Map[KS, R]
    def hasSubResults: Boolean
    def updateValue(path: List[_], newValue: ResultValue): Unit
    def updateDetail[DT](path: List[_], newValue: DT): Unit
    def lookUp(path: List[_]): Result[_, _, _]
    override def toString(): String = Result.prettyPrint(this)
}

case class BaseResult[D](var detail: D, var value: ResultValue) extends Result[D, Nothing, Nothing] {
    def updateValue(path: List[_], newValue: ResultValue) {
        require(path == Nil, "Can't update result at path: " + path + " in BaseResult.")
        require(value == Undecidet, "Can't update non 'Undecidet' result! Old value: " + value + ", new value: " + newValue)
        value = newValue
    }
    def updateDetail[DT](path: List[_], newValue: DT) {
        require(path == Nil, "Can't update result at path: " + path + " in BaseResult.")
        
        (newValue: @unchecked) match {
            case d: D =>
                detail = d
            case _ => throw new IllegalArgumentException("Can't update detail! Incompatible Types!")
        }
    }
    def lookUp(path: List[_]) = {
        require(path == Nil, "Can't lookup result at path: " + path + " in BaseResult.")
        this
    }
    def hasSubResults = false
    def subResults = Map()

}
abstract case class ComposedResult[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](var detail: D, subResults: Map[SK, SR]) extends Result[D, SK, SR] {
    def updateValue(path: List[_], newValue: ResultValue) {
        (path: @unchecked) match {
            case (car: SK) :: cdr => subResults(car).updateValue(cdr, newValue)
            case Nil              => throw new IllegalArgumentException("Can't update composed result with Nil path.")
        }
    }
    def updateDetail[DT](path: List[_], newValue: DT) {
        (path: @unchecked) match {
            case (car: SK) :: cdr => subResults(car).updateDetail(cdr, newValue)
            case Nil =>
                newValue match {
                    case d: D =>
                        detail = d
                    case _ => throw new IllegalArgumentException("Can't update detail! Incompatible Types!")
                }
        }
    }
    def lookUp(path: List[_]): Result[_, _, _] = {
        path match {
            case (car: SK) :: cdr => subResults(car).lookUp(cdr)
            case Nil              => this
        }
    }
    def hasSubResults = !subResults.isEmpty
}

class ConjunctiveResult[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](detail0: D, override val subResults: Map[SK, SR]) extends ComposedResult[D, SK, SR, SD, SSK, SSR](detail0, subResults) {
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
    def apply[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](detail: D, subResults: Map[SK, SR]): ConjunctiveResult[D, SK, SR, SD, SSK, SSR] = new ConjunctiveResult[D, SK, SR, SD, SSK, SSR](detail, subResults)
    def unapply[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](cr: ConjunctiveResult[D, SK, SR, SD, SSK, SSR]): Option[(D, Map[SK, SR])] = {
        Some(cr.detail, cr.subResults)
    }
}

class DisjunctiveResult[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](detail0: D, override val subResults: Map[SK, SR]) extends ComposedResult[D, SK, SR, SD, SSK, SSR](detail0, subResults) {
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
    def apply[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](detail: D, subResults: Map[SK, SR]): DisjunctiveResult[D, SK, SR, SD, SSK, SSR] = new DisjunctiveResult[D, SK, SR, SD, SSK, SSR](detail, subResults)
    def unapply[D, SK, SR <: Result[SD, SSK, SSR], SD, SSK, SSR](cr: DisjunctiveResult[D, SK, SR, SD, SSK, SSR]): Option[(D, Map[SK, SR])] = {
        Some(cr.detail, cr.subResults)
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
    def prettyPrint(res: Result[_, _, _], intention: String = "", prefix: String = ""): String = {
        res match {
            case BaseResult(_, value) => intention + prefix + ": " + value.toString + "\n"
            case v: ComposedResult[_, _, _, _, _, _] => {
                val buf = new StringBuilder()
                buf append intention
                buf append prefix
                buf append ": "
                buf append v.value
                buf append "\n"
                for ((k, sr) <- v.subResults)
                    buf append (prettyPrint(sr, intention + "  ", k.toString()))
                buf toString
            }
        }
    }
}

