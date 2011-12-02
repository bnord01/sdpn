package de.wwu.sdpn.mnfs

import java.io.FileWriter
import java.io.IOException
import java.io.File
import scala.collection.{Set,Map}

object MNFS2Datalog {

    def generateDatalog[P, G, S, T](mnfs: MNFS[P, G, S, T]): StringBuffer = {
        System.err.println("THIS MEHTOD (generateDatalog(mnfs)) IS JUST FOR TESTING!")
        var pmap = Map[P, Int]()
        var gmap = Map[G, Int]()
        var tmap = Map[T, Int]()
        var smap = Map[S, Int]()
        var spmap = Map[(S, P), Int]()

        var index = 0
        for (p <- mnfs.getPSet) {
            pmap += p -> index
            index += 1
        }
        index = 0
        for (g <- mnfs.getGSet) {
            gmap += g -> index
            index += 1
        }

        index = 0
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

        return generateDatalog(mnfs, pmap, gmap, tmap, smap, spmap)
    }
    
    def generateMaps[P, G, S, T](mnfs: MNFS[P, G, S, T]) = {
    	var tmap = Map[T, Int]()
        var smap = Map[S, Int]()
        var spmap = Map[(S, P), Int]()
        val sstringb = new StringBuffer();
    	val tstringb = new StringBuffer();
    	
        var index = 0
        for (s <- mnfs.getSSet) {
            smap += s -> index
            sstringb.append("S_" + s + "\n")
            index += 1
        }

        index = 0
        for (t <- mnfs.getTSet) {
            tmap += t -> index
            tstringb.append ("T_" + t + "\n")
            index += 1
        }
        for (s <- mnfs.getSSet; p <- mnfs.getPSet) {
            spmap += (s, p) -> index
            tstringb.append("SP_" + s + "_" + p + "\n")
            index += 1
        }
        (tmap,smap,spmap,sstringb.toString,tstringb.toString)
    }

    def generateDatalog[P, G, S, T](mnfs: MNFS[P, G, S, T], pmap: Map[P, Int], gmap: Map[G, Int]): StringBuffer = {
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

        return generateDatalog(mnfs, pmap, gmap, tmap, smap, spmap)
    }

    def generateDatalog[P, G, S, T](
        mnfs: MNFS[P, G, S, T],
        pmap: Map[P, Int],
        gmap: Map[G, Int],
        tmap: Map[T, Int],
        smap: Map[S, Int],
        spmap: Map[(S, P), Int]): StringBuffer = {
        val result = new StringBuffer();

        import result.{ append => out }
        
        out(".include \"dpn.datalog\"\n\n")
        out("###DEFINING VARS###\n")
        out("#GLOBAL " + mnfs.getPSet.size + "\n")
        out("#STACK " + mnfs.getGSet.size + "\n")
        out("S " + mnfs.getSSet.size + " smap.map\n")
        out("T " + (mnfs.getTSet.size + mnfs.getSSet.size * mnfs.getPSet.size) + " tmap.map\n")

        out("\n###DEFINING RELATIONS###\n")

        out("sp(s:S,p:GLOBAL,t:T)\n")
        //        out("SPT(s:S,p:P,g:G,t:T)\n")
        out("ts(t:T,s:S)\n")
        out("tt(t:T,g:STACK,t:T)\n")
//        out("start(s:S)\n")
        out("final(end:T)\n")

        out("\n###DEFINING RULES###\n")

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

        out("\n###Declaring Final States\n")
        for ((s, p) <- mnfs.getFinalSPSet)
            out("final(" + spmap(s, p) + ").\n")
        for (t <- mnfs.getFinalTSet)
            out("final(" + tmap(t) + ").\n")

        out("\n###BaseSaturation \n" +
            "tt(SP,G,T) :- base_rule(P,G,_,P1,G1), sp(S,P1,SP1), tt(SP1,G1,T), sp(S,P,SP).\n" +
            "###PopSaturation\n" +
            "tt(SP,G,SP1) :- pop_rule(P,G,_,P1), sp(S,P1,SP1), sp(S,P,SP).\n" +
            "###PushSaturation\n" +
            "tt(SP,G,T) :- push_rule(P,G,_,P1,G1,G2), sp(S,P1,SP1), tt(SP1,G1,T1), tt(T1,G2,T), sp(S,P,SP).\n" +
            "###SpawnSaturation\n" +
            "tt(SP,G,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), sp(S,P2,SP2), tt(SP2,G2,T1), ts(T1,S1), sp(S1,P1,S1P1), tt(S1P1,G1,T), sp(S,P,SP).\n")

        out("### Check if Startconf is Accepted\n")

        mnfs.getStart match {
            case Some(s) =>
                mnfs.getStartConf match {
                    case Some((p, g)) =>
                        out("tt( " + spmap(s,p) + ", " + gmap(g) + ", F), final(F)?\n")
                    case None =>
                }
            case None => 
        }

        

        //        for (s <- mnfs.getSSet) {
        //            for (p <- mnfs.getPSet)
        //                out("S_" + s + " -> SP_" + s + "_" + p + "[ label=\"" + p + "\" ];\n")
        //        }

        //            #BaseSaturation
        //SPT(s,p,g,t) :- BASE_RULE(p,g,_,p',g'), SPT(s,p',g',t).
        //
        //#PopSaturation
        //SPS(s,p,g,t) :- POP_RULE(p,g,_,p'),SPS(s,p',s')

        return result;
    }

    def writeDatalogFile[P, G, S, T](mnfs: MNFS[P, G, S, T], pmap: Map[P, Int], gmap: Map[G, Int]): File = {

        if (mnfs == null) {
            throw new IllegalArgumentException("g is null");
        }
        val (smap,tmap,spmap,sstring,tstring) = generateMaps(mnfs)
        
        val dlStringBuffer = generateDatalog(mnfs, pmap, gmap,smap,tmap,spmap)

        try {
            var f = new File("target/datalog/mnfs.datalog");
            var fw = new FileWriter(f);
            fw.write(dlStringBuffer.toString());
            fw.close();
            
            f = new File("target/datalog/smap.map");
            fw = new FileWriter(f);
            fw.write(sstring);
            fw.close();
            
            f = new File("target/datalog/tmap.map");
            fw = new FileWriter(f);
            fw.write(tstring);
            fw.close();
            return f;
            

        } catch {
            case e: Exception =>
            	e.printStackTrace();
                throw new RuntimeException("Error writing datalog file ");
        }
    }

    def writeDatalogFile[P, G, S, T](mnfs: MNFS[P, G, S, T], datalogfile: String): File = {

        if (mnfs == null) {
            throw new IllegalArgumentException("g is null");
        }
        
        val dlStringBuffer = generateDatalog(mnfs)

        // retrieve the filename parameter to this component, a String
        if (datalogfile == null) {
            throw new IOException("internal error: null filename parameter");
        }
        try {
            val f = new File(datalogfile);
            val fw = new FileWriter(f);
            fw.write(dlStringBuffer.toString());
            fw.close();
            return f;       
            

        } catch {
            case e: Exception =>
                throw new RuntimeException("Error writing datalog file " + datalogfile);
        }
    }

}