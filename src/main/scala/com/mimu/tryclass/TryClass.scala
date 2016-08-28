package com.mimu.tryclass

import java.net.{MalformedURLException, URL}

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

  val ppExc = getURLContent("httppp://finance.yahoo.com") match {
    case Success(lines) => lines
    case Failure(ex) => List(s"failure ${ex.getClass} ${ex.getMessage}")
  }

  val ppRecover = getURLContent("httppp://finance.yahoo.com") recover {
    case e:MalformedURLException => "Enter valid URL"
    case _ => getURLContent("http://finance.yahoo.com").get
  }

  pp.foreach(println)
  ppExc.foreach(println)
  ppRecover.foreach(println)

}
