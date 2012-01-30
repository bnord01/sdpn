package de.wwu.sdpn.eclipse.launching.ui

import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer
import de.wwu.sdpn.wala.analyses.datarace._
import org.eclipse.jface.viewers.ILabelProvider
import org.eclipse.swt.graphics.Image
import DataraceAnalysis._
import org.eclipse.jface.viewers.ILabelProviderListener
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jdt.ui.ISharedImages
import com.ibm.wala.analysis.reflection.InstanceKeyWithNode
import com.ibm.wala.ipa.callgraph.propagation.ConstantKey
import com.ibm.wala.types.TypeReference
import de.wwu.sdpn.core.result._
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ssa.SSAGetInstruction
import com.ibm.wala.ssa.SSAPutInstruction

class ResultTreeModel(result: DRResult) extends ITreeContentProvider with ILabelProvider {

    def getRoot: Object = result
    def getTotalResult = result.value

    def getTotalCount: Int = {
        var count = 0;
        for ((k, v) <- result.subResults; res <- v.subResults)
            count += 1
        count
    }

    def getPositiveCount: Int = getCount(Positive)
    def getNegativeCount: Int = getCount(Negative)

    def getCount(value: ResultValue): Int = {
        var count = 0;
        for ((k, v) <- result.subResults; (k1, res) <- v.subResults; if res.value == value)
            count += 1
        count
    }

    def getElements(inputElement: Object): Array[Object] = {
        inputElement match {
            case null => Array()
            case dbr: DRBaseResult =>
                val rv: Array[Object] = new Array(dbr.detail.size)
                for ((kv, nr) <- dbr.detail.zipWithIndex)
                    rv(nr) = kv
                rv
            case res: Result[AnyRef, AnyRef, AnyRef, AnyRef] => {
                val rv: Array[Object] = new Array(res.subResults.size)
                for ((kv, nr) <- res.subResults.zipWithIndex)
                    rv(nr) = kv._2
                rv
            }
        }
    }

    def getChildren(parentElement: Object): Array[Object] = {
        getElements(parentElement)
    }

    def getParent(element: Object): Object = {
        null
    }

    def hasChildren(element: Object): Boolean = {
        element match {
            case dbr: DRBaseResult => !dbr.detail.isEmpty
            case res: Result[_, _, _, _]                          => res.hasSubResults
            case (node: CGNode, instr: SSAFieldAccessInstruction) => false
        }
    }

    def getImage(obj: Object): Image = {
        val si = JavaUI.getSharedImages

        obj match {
            case res: Result[_, _, _, _] => {
                res.value match {
                    case Positive => si.getImage(ISharedImages.IMG_OBJS_PRIVATE)
                    case Negative => si.getImage(ISharedImages.IMG_OBJS_PUBLIC)
                    case _        => si.getImage(ISharedImages.IMG_OBJS_PROTECTED)
                }
            }
            case (node: CGNode, instr: SSAFieldAccessInstruction) =>
                instr match {
                    case _:SSAGetInstruction => si.getImage(ISharedImages.IMG_FIELD_PUBLIC)
                    case _:SSAPutInstruction => si.getImage(ISharedImages.IMG_FIELD_PRIVATE)
                }

        }
    }

    def getText(obj: Object): String = {
        def nn(tr: TypeReference): String = {
            if (tr.getName().toString().startsWith("L"))
                tr.getName().toString.drop(1).replace("/", ".")
            else
                tr.getName().toString()
        }
        obj match {
            case drr: DRResult     => "Overview"
            case fr: DRFieldResult => "Field: " + nn(fr.key.getDeclaringClass()) + "." + fr.key.getName() + " of type: " + nn(fr.key.getFieldType())
            case br: DRBaseResult => br.key match {
                case nk: InstanceKeyWithNode => "Object created in: " + nk.getNode().getMethod().getSignature()
                case ck: ConstantKey[_]      => "Field on static object: " + ck.getValue()
                case k                       => k.toString()
            }
            case (node: CGNode, instr: SSAFieldAccessInstruction) => instr match {
                    case _:SSAGetInstruction => "Field read in " + node.getMethod().getSignature()
                    case _:SSAPutInstruction => "Field write in " + node.getMethod().getSignature()
                }
        }
    }

    def dispose(): Unit = {}

    def inputChanged(viewer: Viewer, oldInput: Object, newInput: Object): Unit = {}

    def addListener(listener: ILabelProviderListener) {

    }

    def isLabelProperty(element: Object, property: String): Boolean = false

    /**
     * Removes a listener to this label provider.
     * Has no effect if an identical listener is not registered.
     *
     * @param listener a label provider listener
     */
    def removeListener(listener: ILabelProviderListener) {

    }

}