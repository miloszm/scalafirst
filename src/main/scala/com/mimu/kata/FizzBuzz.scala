package com.mimu.kata

/**
  * 28/08/2016.
  */
object FizzBuzz {
  def generate2(i: Int):String = {
    def fizzBuzz(n:Int, s:String)(i:Int):Option[String] =
      if ((i % n == 0) || i.toString.contains(n.toString)) Some(s) else None
    val fizz = fizzBuzz(3,"Fizz")(_)
    val buzz = fizzBuzz(5,"Buzz")(_)
    val list = for {
      Some(fb) <- List(fizz(i),buzz(i))
    } yield fb
    if (list.isEmpty) i.toString else list.mkString
  }

  def generate3(i: Int):String = {
    def fizzBuzz(n:Int, s:String)(i:Int):Option[String] =
      if ((i % n == 0) || i.toString.contains(n.toString)) Some(s) else None
    val fizz = fizzBuzz(3,"Fizz")(_)
    val buzz = fizzBuzz(5,"Buzz")(_)
    val list = List(fizz,buzz).map(_(i)).collect{case(Some(s)) => s}
    if (list.isEmpty) i.toString else list.mkString
  }

  def generate(i: Int):String = {
    def fizzBuzz(n:Int, s:String)(i:Int):Option[String] =
      if ((i % n == 0) || i.toString.contains(n.toString)) Some(s) else None
    val list = fizzBuzz(3,"Fizz")(i).toList ::: fizzBuzz(5,"Buzz")(i).toList
    if (list.isEmpty) i.toString else list.mkString
  }

}
