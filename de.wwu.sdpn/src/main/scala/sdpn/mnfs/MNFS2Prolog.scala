package sdpn.mnfs

import java.io.FileWriter
import java.io.IOException
import java.io.File
import scala.collection.{ Set, Map }

object MNFS2Prolog {

    def generateMaps[P, G, S, T](mnfs: MNFS[P, G, S, T]) = {
        var tmap = Map[T, Int]()
        var smap = Map[S, Int]()
        var spmap = Map[(S, P), Int]()

        var index = 0
        for (s <- mnfs.getSSet) {
            smap += s -> index
            index += 1
        }

        index = 0
        for (t <- mnfs.getTSet) {
            tmap += t -> index
            index += 1
        }
        for (s <- mnfs.getSSet; p <- mnfs.getPSet) {
            spmap += (s, p) -> index
            index += 1
        }
        (tmap, smap, spmap)
    }

    def generateProlog[P, G, S, T](mnfs: MNFS[P, G, S, T], pmap: Map[P, Int], gmap: Map[G, Int]): StringBuffer = {
        var tmap = Map[T, Int]()
        var smap = Map[S, Int]()
        var spmap = Map[(S, P), Int]()

        var index = 0
        for (s <- mnfs.getSSet) {
            smap += s -> index
            index += 1
        }

        index = 0
        for (t <- mnfs.getTSet) {
            tmap += t -> index
            index += 1
        }
        for (s <- mnfs.getSSet; p <- mnfs.getPSet) {
            spmap += (s, p) -> index
            index += 1
        }

        return generateProlog(mnfs, pmap, gmap, tmap, smap, spmap)
    }

    def generateProlog[P, G, S, T](
        mnfs: MNFS[P, G, S, T],
        pmap: Map[P, Int],
        gmap: Map[G, Int],
        tmap: Map[T, Int],
        smap: Map[S, Int],
        spmap: Map[(S, P), Int]): StringBuffer = {
        val result = new StringBuffer();

        import result.{ append => out }

        out("%%% SETTING relation tt tabled %%%\n" +
            ":- table(tt/3).\n")

        out("\n%%%Defining automat%%%\n")

        for ((s, p, g, t) <- mnfs.getSPTSet) {
            out("tt( " + spmap(s, p) + ", " + gmap(g) + ", " + tmap(t) + ").\n")
        }

        for ((s1, p, s2) <- mnfs.getSPSSet) {
            out("ts( " + spmap(s1, p) + ", " + smap(s2) + ").\n")
        }

        for ((t, s) <- mnfs.getTSSet) {
            out("ts( " + tmap(t) + ", " + smap(s) + ").\n")
        }

        for ((t1, g, t2) <- mnfs.getTTSet) {
            out("tt( " + tmap(t1) + ", " + gmap(g) + ", " + tmap(t2) + ").\n")
        }

        for (s <- mnfs.getSSet; p <- mnfs.getPSet) {
            out("sp( " + smap(s) + ", " + pmap(p) + ", " + spmap(s, p) + ").\n")
        }

        out("\n%%%Declaring Final States%%%%\n")
        for ((s, p) <- mnfs.getFinalSPSet)
            out("final(" + spmap(s, p) + ").\n")
        for (t <- mnfs.getFinalTSet)
            out("final(" + tmap(t) + ").\n")

        out("\n%%%BaseSaturation%%% \n" +
            "tt(SP,G,T) :- base_rule(P,G,_,P1,G1), sp(S,P1,SP1), tt(SP1,G1,T), sp(S,P,SP).\n" +
            "%%%PopSaturation%%%\n" +
            "tt(SP,G,SP1) :- pop_rule(P,G,_,P1), sp(S,P1,SP1), sp(S,P,SP).\n" +
            "%%%PushSaturation%%%\n" +
            "tt(SP,G,T) :- push_rule(P,G,_,P1,G1,G2), sp(S,P1,SP1), tt(SP1,G1,T1), tt(T1,G2,T), sp(S,P,SP).\n" +
            "%%%SpawnSaturation%%%\n" +
            "tt(SP,G,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), sp(S,P2,SP2), tt(SP2,G2,T1), ts(T1,S1), sp(S1,P1,S1P1), tt(S1P1,G1,T), sp(S,P,SP).\n")

        mnfs.getStart match {
            case Some(s) =>
                mnfs.getStartConf match {
                    case Some((p, g)) =>
                        out("%%% Check if Startconf is Accepted\n")
                        out("conflict :- cputime(X0), tt( " + spmap(s, p) + ", " + gmap(g) + ", F), final(F),\n" +
                            " cputime(X1), X is X1 - X0, write('cputime: '), write(X), nl.\n")
                    case None =>
                        out("%%% No startconf defined \n")
                }
            case None =>
                out("%%% No start defined\n")
        }

        return result;
    }

    def writePrologFile[P, G, S, T](mnfs: MNFS[P, G, S, T], pmap: Map[P, Int], gmap: Map[G, Int]): File = {

        if (mnfs == null) {
            throw new IllegalArgumentException("g is null");
        }
        val (smap, tmap, spmap) = generateMaps(mnfs)

        val dlStringBuffer = generateProlog(mnfs, pmap, gmap, smap, tmap, spmap)

        try {
            var f = new File("datalog/mnfs.prolog");
            var fw = new FileWriter(f);
            fw.write(dlStringBuffer.toString());
            fw.close();
            return f;

        } catch {
            case e: Exception =>
                throw new RuntimeException("Error writing prolog file ");
        }
    }

}