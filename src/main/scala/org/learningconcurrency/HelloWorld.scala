package org.learningconcurrency

object HelloWrold extends App {
  val lock = new AnyRef
  var message: Option[String] = None
}
