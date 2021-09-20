# sDPN - A Dynamic Pushdown Network Analysis Library
sDPN is a library for program analyses based on dynamic pushdown networks, that is networks of pushdown automata that can spawn new automata and communicate via locks. 

The project consists of the following components:

### de.wwu.sdpn.core
Contains the main analysis, including DPN representation, translation to tree automata, emptiness checks and more.

### de.wwu.sdpn.wala
Contains the Java Bytecode frontend based on the [WALA](http://wala.sourceforge.net).

### de.wwu.sdpn.eclipse 
Contains an eclipse plugin for data race detection in Java programs.

### de.wwu.sdpn.gui
Contains a GUI to explore counterexamples obtained for invalid properties.

### de.wwu.sdpn.pgf
Contains an alternative analysis backend based on parallel flow graphs.

### de.wwu.sdpn.testapps
Contains a set of example Java applications for test purposes. 
