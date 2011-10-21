package de.wwu.sdpn.zzz.ta
import de.wwu.sdpn.ta.ScriptTreeAutomata

trait LockTreeAutomataAlphabet extends ScriptTreeAutomata{
	final val alphabet = Map(
			"nil" -> (2,1),
			"ret" -> (0,1),
			"base" -> (0,2),
			"call1" -> (0,2),
			"call2" -> (0,3),
			"use" -> (1,3),
			"acq" -> (1,2),
			"spawn" -> (0,3)			
			)
}