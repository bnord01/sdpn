package de.wwu.sdpn.wala.analyses
import java.io.BufferedInputStream
import java.util.Properties

/**
 * Properties used for various analyses 
 * 
 * @param xsbExe full path to the XSB executable
 * @param tempDir path to the directory used for temp files
 * @param classPath class path to analyze 
 * @param mainClass the main class to analyze 
 * @param exclusiveMethod the method to check for mutual exclusion and slicing
 * @param lockSens should the analysis be lock sensitive
 * @param lockType the lock type used for filtering
 * @param witness should a witness be generated
 * @param slicing should slicing be used
 * @param debug should debug messages be printed 
 * 
 * @author Benedikt Nordhoff
 * 
 */
case class SDPNTestProps(
    classPath: String,
    mainClass: String,
    exclusiveMethod: String,
    lockSens: Boolean,
    lockType: String,
    witness: Boolean,
    slicing: Boolean)

object SDPNTestProps{
    /**
     * Read the default properties from the {{sdpn.test.properties}} file found on the class path
     */
    lazy val get: SDPNTestProps = {
        val f = this.getClass.getClassLoader.getResourceAsStream("sdpn.test.properties")
        val fin = new BufferedInputStream(f)
        val p = new Properties()
        p.load(fin)       
        
        val cp: String = p.getProperty("std_cp")
        val mc: String = p.getProperty("std_mc")
        val ex: String = p.getProperty("std_ex")
        val ls: Boolean = p.getProperty("std_lockinsensitive") == null
        val lt: String = p.getProperty("std_lt")
        val witness: Boolean = p.getProperty("std_witness") != null
        val slicing: Boolean = p.getProperty("std_noslice") == null

        SDPNTestProps(
            cp,
            mc,
            ex,
            ls,
            lt,
            witness,
            slicing)
    }
}


