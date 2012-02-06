package de.wwu.sdpn.core.analyses

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.util.Properties
import java.io.File
import java.io.InputStream

/**
 * Properties used for various analyses
 *
 * @param xsbExe full path to the XSB executable
 * @param tempDir path to the directory used for temp files
 * @param debug should debug messages be printed
 *
 * @author Benedikt Nordhoff
 *
 */
case class SDPNProps(
    xsbExe: String,
    tempDir: String,
    debug: Boolean)

object SDPNProps {
    var defaultProps: SDPNProps = null;
    /**
     * Read the default properties from the {{sdpn.properties}} file found on the class path
     */
    def get: SDPNProps = {
        if (defaultProps == null) {
            val stream = this.getClass().getClassLoader().getResourceAsStream("sdpn.properties")
            defaultProps = get(stream)
        }
        return defaultProps
    }
    def setXSB(exe:String) {
        defaultProps = get.copy(xsbExe = exe)
    }
    def setTempDir(dir:String) {
        defaultProps = get.copy(tempDir = dir)       
    }
    def setDebug(value:Boolean) {
        defaultProps = get.copy(debug = value)
    }
    
    def set(props:SDPNProps) {
        defaultProps = props
    }

    def get(stream: InputStream): SDPNProps = {
        val fin = new BufferedInputStream(stream)
        val p = new Properties()
        p.load(fin)

        val xsbExe: String = p.getProperty("xsb_exe")
        val xsbFile = new File(xsbExe)
        assert(xsbFile.exists(), "Declared XSB executable doesn't exist.")
        assert(xsbFile.canExecute(), "Declared XSB executable isn't executable.")
        val tempDir: String = p.getProperty("temp_dir")
        val tempFile = new File(tempDir)
        if (!tempFile.exists)
            tempFile.mkdir()
        assert(tempFile.exists(), "Couldn't create tempDir: " + tempFile.getAbsolutePath + " .")
        assert(tempFile.isDirectory(), "Declared tempDir: " + tempFile.getAbsolutePath + " isn't a directory.")

        val debug: Boolean = p.getProperty("debug") != null

        return SDPNProps(
            xsbExe,
            tempDir,
            debug)
    }
}


