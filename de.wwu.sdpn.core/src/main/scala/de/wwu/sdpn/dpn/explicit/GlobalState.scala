package de.wwu.sdpn.dpn.explicit

sealed trait GlobalState {
	def num:Int
}

case object NState extends GlobalState{val num = 0}
case object EState extends GlobalState{val num = 1}