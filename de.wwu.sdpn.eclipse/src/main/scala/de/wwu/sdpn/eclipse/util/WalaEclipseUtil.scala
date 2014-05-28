package de.wwu.sdpn.eclipse.util

import java.util.StringTokenizer
import java.util.jar.JarFile
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IMethod
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.Signature
import com.ibm.wala.classLoader.BinaryDirectoryTreeModule
import com.ibm.wala.classLoader.SourceDirectoryTreeModule
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.AnalysisScope
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.propagation.SSAPropagationCallGraphBuilder
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.properties.WalaProperties
import com.ibm.wala.types.TypeReference
import com.ibm.wala.util.io.FileProvider
import com.ibm.wala.util.strings.Atom
import de.wwu.sdpn.wala.util.ThreadSensContextSelector
import java.io.ByteArrayInputStream
import com.ibm.wala.util.config.FileOfClasses

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
    
    def makeScope(scopeData: List[String], javaLoader: ClassLoader = this.getClass.getClassLoader): AnalysisScope = {
    val scope = AnalysisScope.createJavaAnalysisScope()
    for (entry <- scopeData) {
      val token = new StringTokenizer(entry, "\n,")
      val loaderName = Atom.findOrCreateUnicodeAtom(token.nextToken());
      val walaLoader = scope.getLoader(loaderName)
      assert(walaLoader != null, "Did not find loader '" + loaderName + "' ")
      val lang = token.nextToken()
      assert(lang.equals("Java"), "Expected language Java but got: " + lang)
      val entryType = token.nextToken()
      val entryPathname = token.nextToken();
      entryType match {
        case "classFile" =>
          val cf = (new FileProvider()).getFile(entryPathname, javaLoader);
          scope.addClassFileToScope(walaLoader, cf);
        case "sourceFile" =>
          val sf = (new FileProvider()).getFile(entryPathname, javaLoader);
          scope.addSourceFileToScope(walaLoader, sf, entryPathname);
        case "binaryDir" =>
          val bd = (new FileProvider()).getFile(entryPathname, javaLoader);
          assert(bd.isDirectory(), "Specified binary dir isn't a directory: " + entryPathname)
          scope.addToScope(walaLoader, new BinaryDirectoryTreeModule(bd));
        case "sourceDir" =>
          val sd = (new FileProvider()).getFile(entryPathname, javaLoader);
          assert(sd.isDirectory(), "Specified source dir isn't a directory: " + entryPathname)
          scope.addToScope(walaLoader, new SourceDirectoryTreeModule(sd));
        case "jarFile" =>
          val M = (new FileProvider()).getJarFileModule(entryPathname,
            javaLoader);
          scope.addToScope(walaLoader, M);
        case "stdlib" =>
          val stdlibs = WalaProperties.getJ2SEJarFiles();
          for (libentry <- stdlibs) {
            scope.addToScope(walaLoader, new JarFile(libentry));
          }
        case _ => throw new IllegalArgumentException("Unknown entry type: " + entryType)
      }
    }

    val exclusions =
      """java/awt/.*
java/security/.*
javax/swing/.*
sun/awt/.*
sun/swing/.*
com/sun/.*
sun/.*"""
    val exBytes = exclusions.getBytes()

    val exStream = new ByteArrayInputStream(exBytes)

    val soc = new FileOfClasses(exStream)

    scope.setExclusions(soc)

    assert(exclusions.length() == exBytes.length, "Error in conversion! :(")
    return scope
  }

  def makeScopeFromCPnJRE(cp: String, jre: String, loader: ClassLoader = this.getClass.getClassLoader): AnalysisScope = {
    val list = List("Primordial,Java,jarFile," + jre, "Application,Java,binaryDir," + cp)
    return makeScope(list, loader)
  }

  def makeCGFromScopeAndMainClass(scope: AnalysisScope, mc: String): (CallGraph, PointerAnalysis[InstanceKey]) = {
    val cha = ClassHierarchy.make(scope);

    val entrypoints = Util.makeMainEntrypoints(scope, cha, mc);

    val options = new AnalysisOptions(scope, entrypoints);

    val cache = new AnalysisCache();

    val cgbuilder: SSAPropagationCallGraphBuilder =
      Util.makeVanillaZeroOneCFABuilder(options, cache, cha, scope, new ThreadSensContextSelector(), null);

    val cg = cgbuilder.makeCallGraph(options)

    val pa = cgbuilder.getPointerAnalysis

    return (cg, pa)
  }

}