package de.wwu.sdpn.wala.util
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.AnalysisScope
import java.util.StringTokenizer
import com.ibm.wala.util.strings.Atom
import com.ibm.wala.util.io.FileProvider
import com.ibm.wala.classLoader.BinaryDirectoryTreeModule
import com.ibm.wala.classLoader.SourceDirectoryTreeModule
import com.ibm.wala.properties.WalaProperties
import java.util.jar.JarFile
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ipa.callgraph.propagation.SSAPropagationCallGraphBuilder
import java.util.LinkedList
import java.io.ByteArrayInputStream
import com.ibm.wala.util.config.FileOfClasses

object WalaUtil {
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

  def makeCGFromScopeAndMainClass(scope: AnalysisScope, mc: String): (CallGraph, PointerAnalysis) = {
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