package sdpn.runner
import scala.Console
import java.io.File
import java.io.IOException
import scala.io.Source
import joptsimple.OptionParser
import scala.collection.JavaConversions._
import com.ibm.wala.types.TypeName
import com.ibm.wala.types.MethodReference

object CLRunner {

    def main(args: Array[String]): Unit = {
        val parser = new OptionParser()

        parser.accepts("mc").withRequiredArg
        parser.accepts("ex").withRequiredArg

        parser.acceptsAll(List("h", "?", "help"), "show help");
        parser.acceptsAll(List("c", "classpath"), "class path to analyze").withRequiredArg()
            .describedAs("path1" + File.pathSeparator + "path2" + File.pathSeparator + "...");
        parser.acceptsAll(List("m", "mainclass"), "class which contains the main-method").withRequiredArg()
            .describedAs("Lpackage/packge/Class")

        parser.acceptsAll(List("e", "excludedMethod"), "signature of method which should be checked for mutal exclusion").withRequiredArg()
            .describedAs("package.package.Class.method()V")

        parser.acceptsAll(List("n", "nolocks"), "use lockinsensitive analysis")

        parser.acceptsAll(List("s", "noslicing"), "don't use slicing")

        parser.acceptsAll(List("l", "locktype"), "type used to filter locks").withRequiredArg()
            .describedAs("Lpackage/packge/Class")

        parser.printHelpOn(System.out);

        val options = parser.parse(args: _*)

    }

}