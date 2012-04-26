package de.wwu.sdpn.pfg.fixedpoint

trait Statement [+T]{
    def lhs: T
    def rhs: List[T]
    /**
     * Evaluate this statement updating the value of LHS and returing information whether 
     * the value of LHS has changed.
     */
    def evaluate(): ChangeInfo
}

trait LHSSubstitution[-T,+S]{
    def substituteLHS(nVar: T):S
}

trait IdentityCheck[+T]{
    def isId :Boolean
    def getIdVar:Option[T]
}


sealed trait ChangeInfo
sealed trait InfoChanged extends ChangeInfo
sealed trait InfoNotChanged extends ChangeInfo
case object Changed extends InfoChanged
case object NotChanged extends InfoNotChanged
case object ChangedAndFixed extends InfoChanged
case object NotChangedAndFixed extends InfoNotChanged