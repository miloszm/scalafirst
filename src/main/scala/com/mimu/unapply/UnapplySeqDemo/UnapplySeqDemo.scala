package com.mimu.unapply.UnapplySeqDemo

/**
  * based on Neophyte's guide to Scala
  */



object UnapplySeqDemo extends App {

  object GivenNames {
    def unapplySeq(name: String): Option[Seq[String]] = {
      val names = name.trim.split("\\s+")
      if (names.forall(_.isEmpty)) None else Some(names)
    }
  }

  object FullNames {
    def unapplySeq(name: String): Option[(String, String, Seq[String])] = {
      val names = name.trim.split("\\s+").toSeq
      if (names.size < 2) None
      else Some((names.last, names.head, names.drop(1).dropRight(1)))
    }
  }

  def greetWithFirstName(name: String) = name match {
    case GivenNames(firstName, secondName) => s"hi $firstName $secondName"
    case GivenNames(firstName, _*) => s"hi $firstName"
    case _ => "fallback"
  }

  def greetWithFullName(name: String) = name match {
    case FullNames(firstName, secondName, rest, rest2) => s"hi $firstName $secondName $rest $rest2"
    case FullNames(firstName, secondName, rest) => s"hi $firstName $secondName $rest"
    case _ => "fallback"
  }


  println(greetWithFirstName("Jean Claude"))
  println(greetWithFirstName("Jacques"))
  println(greetWithFirstName("Jean Claude Marie"))

  println(greetWithFullName("Jean Claude Philip Marie"))
  println(greetWithFullName("Jean Claude Marie"))

}
