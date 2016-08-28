package com.mimu.unapply

/**
  * based on Neophyte's guide to Scala
  */
trait User {
  def name: String
  def score: Int
}

class FreeUser(val name:String, val score:Int) extends User

class PremiumUser(val name:String, val score:Int) extends User

object FreeUser {
  def unapply(user: FreeUser): Option[(String,Int)] = Some((user.name,user.score))
}

object PremiumUser {
  def unapply(user: PremiumUser): Option[(String,Int)] = Some((user.name,user.score))
}

object SuperUser {
  def unapply(user: PremiumUser): Boolean = user.score > 5
}


object UnapplyDemo extends App {
  val user: User = new PremiumUser("Daniel", 6) // try 4
  val u = user match {
    case FreeUser(name,_) => s"Hello name"
    case su @ SuperUser() => s"Hi Superuser ${su.name}"
    case PremiumUser(name,score) => s"Welcome back, dear $name with score $score"
    case _ => "No bonus"
  }
  println(u)
}
