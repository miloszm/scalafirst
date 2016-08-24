package com.mimu.fpinscala.ch5

object InfiniteStreams extends App {

  /**
    * 5.8
    */
  def constant[A](a:A): Stream[A] = {
    lazy val s:Stream[A] = Stream.cons(a, s)
    s
  }

  /**
    * 5.9
    */
  def from(n:Int): Stream[Int] = {
    lazy val s:Stream[Int] = Stream.cons(n, from(n+1))
    s
  }

  println(constant(5).take(6).toList)
  println(from(5).take(6).toList)

}

//import com.mimu.fpinscala.ch5.InfiniteStreams._
