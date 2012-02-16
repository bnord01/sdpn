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
     * Read the default properties from the {{sdpn.properties}} file 
     * defined by the system property `sdpn.properties` 
     * or found in the current directory or found on the class path.
     * In that order.
     */
    def get: SDPNProps = {
        if (defaultProps == null) {
            val propfile = System.getProperty("sdpn.properties")
            if (propfile != null) {
                val file = new File(propfile)
                assert(file.exists() && file.canRead(), "Can't read sdpn.properties file: " + propfile)
                val instream = new BufferedInputStream(new FileInputStream(file));
                defaultProps = get(instream)
                return defaultProps
            }
            val file = new File("sdpn.properties")
            if (file.exists()) {
                assert(file.canRead(), "Can't read sdpn.properties file: sdpn.properties")
                val instream = new BufferedInputStream(new FileInputStream(file));
                defaultProps = get(instream)
                return defaultProps
            }

            val stream = this.getClass().getClassLoader().getResourceAsStream("sdpn.properties")
            defaultProps = get(stream)
            return defaultProps

        }
        return defaultProps
    }
    def setXSB(exe: String) {
        defaultProps = get.copy(xsbExe = exe)
    }
    def setTempDir(dir: String) {
        defaultProps = get.copy(tempDir = dir)
    }
    def setDebug(value: Boolean) {
        defaultProps = get.copy(debug = value)
    }

    def set(props: SDPNProps) {
        defaultProps = props
    }

    def get(stream: InputStream): SDPNProps = {
        val fin = new BufferedInputStream(stream)
        val p = new Properties()
        p.load(fin)

        val xsbExe: String = p.getProperty("xsb_exe")
        val xsbFile = new File(xsbExe)
        assert(xsbFile.exists(), "Declared XSB executable doesn't exist: " + xsbExe)
        assert(xsbFile.canExecute(), "Declared XSB executable isn't executable: " + xsbExe)
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


