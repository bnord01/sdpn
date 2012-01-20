package de.wwu.sdpn.eclipse.launching

final object DataraceLaunchConfig {

  final val PROJECT_NAME = "PROJECT_NAME"
  final val PROJECT_NAME_DEFAULT = ""

  final val MAIN_CLASS_NAME = "MAIN_CLASS_NAME"
  final val MAIN_CLASS_NAME_DEFAULT = ""

  final val EXCLUSIONS = "EXCLUSIONS"
  final val NEW_LINE = System.getProperty("line.separator");
  final val EXCLUSIONS_DEFAULT =
    "java/awt/.*" + NEW_LINE +
      "javax/swing/.*" + NEW_LINE +
      "sun/awt/.*" + NEW_LINE +
      "sun/swing/.*" + NEW_LINE +
      "com/sun/.*" + NEW_LINE +
      "sun/.*";

}