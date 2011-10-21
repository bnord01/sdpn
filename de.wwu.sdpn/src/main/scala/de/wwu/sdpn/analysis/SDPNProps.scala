package de.wwu.sdpn.analysis
import com.ibm.wala.util.io.FileProvider
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.util.Properties
import java.io.File

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
case class SDPNProps(
    xsbExe: String,
    tempDir: String,
    classPath: String,
    mainClass: String,
    exclusiveMethod: String,
    lockSens: Boolean,
    lockType: String,
    witness: Boolean,
    slicing: Boolean,
    var debug: Boolean)

object SDPNProps{
    /**
     * Read the default properties from the {{sdpn.properties}} file found on the class path
     */
    lazy val get: SDPNProps = {
        val f = FileProvider.getFile("sdpn.properties")
        val fin = new BufferedInputStream(new FileInputStream(f))
        val p = new Properties()
        p.load(fin)

        val xsbExe: String = p.getProperty("xsb_exe")
        var file = new File(xsbExe)
        assert (file.exists(),"Declared XSB executable doesn't exist.")
        assert (file.canExecute(),"Declared XSB executable isn't executable.")        
        val tempDir: String = p.getProperty("temp_dir")
        file = new File(tempDir)
        assert (file.exists(),"Declared tempDir doesn't exist.")
        assert (file.isDirectory(),"Declared tempDir isn't a directory.")        
        
        val cp: String = p.getProperty("std_cp")
        val mc: String = p.getProperty("std_mc")
        val ex: String = p.getProperty("std_ex")
        val ls: Boolean = p.getProperty("std_lockinsensitive") == null
        val lt: String = p.getProperty("std_lt")
        val witness: Boolean = p.getProperty("std_witness") != null
        val slicing: Boolean = p.getProperty("std_noslice") == null
        val debug: Boolean = p.getProperty("debug") != null

        SDPNProps(
            xsbExe,
            tempDir,
            cp,
            mc,
            ex,
            ls,
            lt,
            witness,
            slicing,
            debug)
    }
}


