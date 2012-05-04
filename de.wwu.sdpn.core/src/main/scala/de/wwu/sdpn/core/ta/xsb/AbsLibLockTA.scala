package de.wwu.sdpn.core.ta.xsb

trait AbsLibLockTA {
    protected def lo: LockOperations
    def name: String

    protected def lon = lo.name

    case class FSym(str: String) {
        def apply(args: String*): String = args.mkString(name + "_" + str + "(", " , ", ")")
    }

    def NIL = FSym("nil")
    def RET = FSym("ret")
    def BASE = FSym("base")
    def CALL1 = FSym("call1")
    def CALL2 = FSym("call2")
    def USE = FSym("use")
    def ACQ = FSym("acq")
    def SPAWN = FSym("spawn")
    def CUT = FSym("cut")
    def FINAL = FSym("final")

    def out(str: String)(implicit sb: StringBuilder) { sb.append(str) }

    implicit def str2ding(str: String) = new Object() {
        def :-(constr: String*) = { str + constr.mkString(" :- \n\t", " ,\n\t", "") }
        def !(implicit sb: StringBuilder): Unit = { sb.append(str); sb.append(".\n") }
    }
    
    def %(str:String) (implicit sb:StringBuilder) = sb.append("% " + str + "\n")
    def %%%(str:String) (implicit sb:StringBuilder) = {sb.append("\n        %%%  ");sb.append(str);sb.append("  %%%\n\n")}

    trait LSVar
    trait LVar
    trait GVar
    trait TBVar
    
    case object top extends TBVar{
        override def toString = "1"            
    }
    case object bot extends TBVar{
        override def toString = "0"            
    }
    case object C extends TBVar
    case object D extends TBVar    

    case object A extends LSVar
    case object U extends LSVar
    case object G extends GVar
    case object A1 extends LSVar
    case object U1 extends LSVar
    case object G1 extends GVar
    case object A2 extends LSVar
    case object U2 extends LSVar
    case object G2 extends GVar
    case object A3 extends LSVar
    case object U3 extends LSVar
    case object G3 extends GVar
    case object Ac extends LSVar
    case object Uc extends LSVar
    case object Gc extends GVar
    case object Ar extends LSVar
    case object Ur extends LSVar
    case object Gr extends GVar
    case object As extends LSVar
    case object Us extends LSVar
    case object Gs extends GVar
    
    case object X extends LVar
    case object L extends LVar
    
    case object __ extends LVar with LSVar with GVar with TBVar {
        override def toString = "_"
    }

    //def isDifference(X:String,Y:String,XmY:String) = XmY + (" is X '/\\' (Y '><' " + allLocks +")").replace("X",X).replace("Y",Y)

    def disjoint(x: LSVar, y: LSVar) = lon + "_disjoint(" + x + "," + y + ")"
    def isUnion(x: LSVar, y: LSVar, xy: LSVar) = lon + "_isUnion(" + x + "," + y + "," + xy + ")"
    def isUnion3(x: LSVar, y: LSVar, z: LSVar, xyz: LSVar) = lon + "_isUnion3(" + x + ", " + y + ", " + z + ", " + xyz + ")"

    def isElem(elem: LVar, set: LSVar) = lon + "_isElem(" + elem + "," + set + ")"
    def isNoElem(elem: LVar, set: LSVar) = lon + "_isNoElem(" + elem + ", " + set + ")"
    def isElemUnion(elem: LVar, set: LSVar, eset: LSVar) = lon + "_isElemUnion(" + elem + "," + set + "," + eset + ")"
    def isElemUnion2(elem: LVar, set1: LSVar, set2: LSVar, eset: LSVar) = lon + "_isElemUnion2(" + elem + "," + set1 + ", " + set2 + ", " + eset + ")"
    def isGraphUnion(g1: GVar, g2: GVar, g3: GVar) =
        lon + "_isGraphUnion(" + g1 + ", " + g2 + ", " + g3 + ")"
    def isGraphXUUnion(x: LVar, u: LSVar, g1: GVar, g2: GVar) =
        lon + "_isGraphXUUnion(" + x + ", " + u + ", " + g1 + ", " + g2 + ")"
    def isGraphXUUnion2(x: LVar, u: LSVar, g1: GVar, g2: GVar, g:GVar) =
        lon + "_isGraphXUUnion2(" + x + ", " + u + ", " + g1 + ", " + g2 + ", " + g +")"
    def emptyGraph(g: GVar) = lon + "_emptyGraph(" + g + ")"
    def emptyLockSet(g: LSVar) = lon + "_emptyLockSet(" + g + ")"
    def notCyclic(g: GVar) = lon + "_notCyclic(" + g + ")"
    def acyclic(g: GVar) = lon + "_notCyclic(" + g + ")"

}