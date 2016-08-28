package com.mimu.tryclass

import java.net.URL

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * from neophyte's guide to scala
  */
object TryClass extends App {

  def parseURL(url:String) = Try{ new URL(url) }

  val p = parseURL("http://finance.yahoo.com").map(_.getProtocol)

  println(p)

  def getURLContent(url:String): Try[Iterator[String]] =
    for {
      url <- parseURL(url)
      connection <- Try(url.openConnection())
      is <- Try(connection.getInputStream)
      source = Source.fromInputStream(is)
    } yield {
      source.getLines()
    }

  val pp = getURLContent("http://finance.yahoo.com") match {
    case Success(lines) => lines
    case Failure(ex) => List(s"failure ${ex.getMessage}")
  }

  pp.foreach(println)

}
