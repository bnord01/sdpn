package de.wwu.sdpn.eclipse.util
import org.eclipse.jdt.core.IType
import com.ibm.wala.types.TypeReference
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.Signature
import org.eclipse.core.runtime.IProgressMonitor
import com.ibm.wala.ipa.callgraph.CGNode
import org.eclipse.jdt.core.IMethod

object WalaEclipseUtil {
    
    def cgnode2IMethod (ip:IJavaProject, cgnode : CGNode) : IMethod = {
        val method = cgnode.getMethod()
        val cr = method.getDeclaringClass().getReference()
        val itype = typeRef2IType(ip,cr)
        var methodName = method.getName().toString()
        if(methodName == "<init>")
            methodName = cr.getName().toString().split("/").last
        val i0 = if(method.isStatic()) 0 else 1
        val params = (for (i <- i0 until method.getNumberOfParameters()) yield {
            Signature.createTypeSignature(typeRef2FullyQuallifiedName(method.getParameterType(i),true),false)
        }) . toArray
        itype.getMethod(methodName,params)
    }

    def typeRef2IType(ip: IJavaProject, tr: TypeReference): IType = {
        val fqsig = typeRef2FullyQuallifiedName(tr)
        //val sig = Signature.createTypeSignature(fqsig,false)
        ip.findType(fqsig,null:IProgressMonitor)        
    }

    def typeRef2FullyQuallifiedName(tr: TypeReference,stripPackage:Boolean=false): String = {
        if (tr.isArrayType())
            typeRef2FullyQuallifiedName(tr.getArrayElementType(),stripPackage) + "[]"
        else if (tr.isPrimitiveType())
            tr.getName().toString() match {
                case "I" => "int"
                case "J" => "long"
                case "V" => "void"
                case "C" => "char"
                case "Z" => "boolean"
                case "D" => "double"
                case "F" => "float"
                case "B" => "byte"
                case "S" => "short"
                case t => throw new MatchError("Expected primitive type I,J,V,C,Z,D,F,B,S but got: " + t)
            }
        else {
        	assert (tr.isClassType(),"Expected array, primitive or class type but got: " + tr)
        	if(stripPackage) 
        	    tr.getName().getClassName().toString()
        		//tr.getName().toString().substring(tr.getName().getPackage().length() + 2, tr.getName().toString().length())
        	else
        		tr.getName().toString().drop(1).replace('/', '.')
        }

    }

}