package sdpn.util

import com.ibm.wala.viz.PDFViewUtil
import com.ibm.wala.viz.DotUtil
import java.io.FileWriter
import java.io.File
import java.io.IOException
import sdpn.mnfs.MNFS
object MNFSPrinter {

    def dotify[P, G, T, S](mnfs: MNFS[P, G, T, S]) {
        val f = writeDotFile(mnfs, null, "mnfs.dot")
        val dotExe = "/usr/bin/dot"
        val pdfFile = "mnfs.pdf"
        val pdfViewExe = "/usr/bin/xdg-open"
        if (dotExe != null) {
            DotUtil.spawnDot(dotExe, pdfFile, f);
            PDFViewUtil.launchPDFView(pdfFile, pdfViewExe);
        }
    }

    val fontSize = 6;
    val fontColor = "black";
    val fontName = "Arial";

    def generateDot[P, G, T, S](mnfs: MNFS[P, G, T, S], title: String): StringBuffer = {
        val result = new StringBuffer("digraph \"DirectedGraph\" {\n");

        if (title != null) {
            result.append("graph [label = \"" + title + "\", labelloc=t, concentrate = false];\n");
        } else {
            result.append("graph [concentrate = false];\n");
        }

        result.append("rankdir=LR;\n");

        val fontsizeStr = "fontsize=" + fontSize;
        val fontcolorStr = if (fontColor != null) ",fontcolor=" + fontColor else "";
        val fontnameStr = if (fontName != null) ",fontname=" + fontName else "";

        result.append("center=true;\n");
        result.append(fontsizeStr + ";\n");
        result.append("node [ color=blue,shape=\"circle\"");
        result.append(fontsizeStr);
        result.append(fontcolorStr);
        result.append(fontnameStr);
        result.append("];\n")
        result.append("edge [ color=black,");
        result.append(fontsizeStr);
        result.append(fontcolorStr);
        result.append(fontnameStr);
        result.append("]; \n");

        for (s <- mnfs.getSSet) {
            result.append("S_" + s + " ")
            for (p <- mnfs.getPSet)
                result.append("SP_" + s + "_" + p + " ")
            result.append("; \n");
        }

        for (t <- mnfs.getTSet) {
            result.append("T_" + t + " ")
        }
        result.append("; \n");

        for ((s, p, g, t) <- mnfs.getSPTSet) {
            result.append("SP_" + s + "_" + p + " -> T_" + t + " [ label=\"" + g + "\" ];\n")
        }

        for ((s1, p, s2) <- mnfs.getSPSSet) {
            result.append("SP_" + s1 + "_" + p + " -> S_" + s2 + " ;\n")
        }

        for ((t, s) <- mnfs.getTSSet) {
            result.append("T_" + t + " -> S_" + s + " ;\n")
        }

        for ((t1, g, t2) <- mnfs.getTTSet) {
            result.append("T_" + t1 + " -> T_" + t2 + " [ label=\"" + g + "\" ];\n")
        }

        for (s <- mnfs.getSSet) {
            for (p <- mnfs.getPSet)
                result.append("S_" + s + " -> SP_" + s + "_" + p + "[ label=\"" + p + "\" ];\n")
        }

        result.append("\n}");
        return result;
    }

    def writeDotFile[P, G, S, T](mnfs: MNFS[P, G, S, T], title: String, dotfile: String): File = {

        if (mnfs == null) {
            throw new IllegalArgumentException("g is null");
        }
        val dotStringBuffer = generateDot(mnfs, title)

        // retrieve the filename parameter to this component, a String
        if (dotfile == null) {
            throw new IOException("internal error: null filename parameter");
        }
        try {
            val f = new File(dotfile);
            val fw = new FileWriter(f);
            fw.write(dotStringBuffer.toString());
            fw.close();
            return f;

        } catch {
            case e: Exception =>
                throw new RuntimeException("Error writing dot file " + dotfile);
        }
    }

}