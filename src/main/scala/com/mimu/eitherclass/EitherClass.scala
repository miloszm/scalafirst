package com.mimu.eitherclass

import java.net.{MalformedURLException, URL}

import scala.util.control.Exception.catching

/**
  * from neophyte's guide to scala
  */
object EitherClass extends App {

  def handling[Ex <: Throwable,T]( exType:Class[Ex])(block: => T): Either [Ex,T] =
    catching(exType).either(block).asInstanceOf[Either[Ex,T]]

  def parseURL(url:String): Either[MalformedURLException, URL] =
    handling(classOf[MalformedURLException])(new URL(url))

  def parseURL2(url:String): Either[MalformedURLException, URL] = {
    try {
      Right(new URL(url))
    }
    catch {
      case mue:MalformedURLException => Left(mue)
    }
  }

  def doTest(f:String => Either[MalformedURLException, URL]):Unit =
  {
    val p = f("httppp:finance.yahoo.com")
    val pp = f("http:finance.yahoo.com")
    println(p)
    println(pp)
  }

  doTest(parseURL)
  doTest(parseURL2)

}
