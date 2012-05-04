package de.wwu.sdpn.core.ta.xsb

trait LockOperations extends XSBScript{
  def name:String
  def genScript:String
  def boundNames = Set(name)
}