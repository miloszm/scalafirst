package com.mimu.fpinscala.ch5

import scala.Stream._

object InfiniteStreams extends App {

  /**
    * 5.8
    */
  def constant[A](a:A): Stream[A] = {
    lazy val s:Stream[A] = cons(a, s)
    s
  }

  /**
    * 5.9
    */
  def from(n:Int): Stream[Int] = {
    lazy val s:Stream[Int] = cons(n, from(n+1))
    s
  }

  /**
    * 5.10
    */
  def fib: Stream[Int] = {
    def go(n:Int):Int = n match {
      case 0 => 0
      case 1 => 1
      case x => go(x-1) + go(x-2)
    }
    def s(n:Int):Stream[Int] = cons(go(n), s(n+1))
    s(0)
  }
  val fib2: Stream[Int] = {
    def go(n1:Int, n2:Int):Stream[Int] = {
      cons(n1, go(n2, n1+n2))
    }
    go(0,1)
  }

  /**
    * 5.11
    */
  def unfold[A,S](z:S)(f: S => Option[(A,S)]): Stream[A] = {
    def go(s:S):Stream[A] = {
      f(s) match {
        case Some((a,ss)) => cons(a, go(ss))
        case None => empty
      }
    }
    go(z)
  }

  /**
    * 5.12
    */
  def fromUnfold(n:Int):Stream[Int] = {
    unfold[Int,Int](n)(a => Some(a, a + 1))
  }
  def constantUnfold(n:Int):Stream[Int] = {
    unfold[Int,Int](n)(a => Some(a, a))
  }
  def fibUnfold():Stream[Int] = {
    def f(p:(Int,Int)):Option[(Int, (Int,Int))] = p match {
      case (a,b) => Some(a,(b,a+b))
    }
    unfold[Int,(Int,Int)]((0,1))(f)
  }
  def fibUnfold2() = unfold((0,1)){ case (a,b) => Some((a, (b, a+b)))}

  println(constant(5).take(6).toList)
  println(constantUnfold(5).take(6).toList)
  println
  println(from(5).take(6).toList)
  println(fromUnfold(5).take(6).toList)
  println
  println(fib.take(10).toList)
  println(fib2.take(10).toList)
  println(fibUnfold.take(10).toList)
  println(fibUnfold2.take(10).toList)

  /**
    * 5.13
    */
  def mapUnfold[A,B](s:Stream[A])(f:A => B): Stream[B] = {
    unfold(s)(ss => if (ss.isEmpty) None else Some(f(ss.head),ss.tail))
  }
  def mapUnfold2[A,B](s:Stream[A])(f:A => B): Stream[B] = {
    unfold(s){
      case cons(h,t) => Some(f(h),t)
      case _ => None
    }
  }
  def takeUnfold[A](s:Stream[A], n:Int): Stream[A] = {
    unfold((s,n))(ss => if (ss._1.isEmpty || ss._2 == 0) None else Some(ss._1.head,(ss._1.tail,ss._2-1)))
  }
  def takeUnfold2[A](s:Stream[A], n:Int): Stream[A] = {
    unfold((s,n)){
      case (cons(h,t),x) if (x > 0) => Some(h,(t, x-1))
      case (_,x) if (x == 0) => None
    }
  }
  def takeWhileUnfold[A](s:Stream[A])(f: A => Boolean): Stream[A] = {
    unfold(s)(ss => if (ss.isEmpty || !f(ss.head)) None else Some(ss.head, ss.tail))
  }
  def zipWithUnfold[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C):Stream[C] = {
    unfold((a,b))(ss => if (ss._1.isEmpty || ss._2.isEmpty) None else Some(f(ss._1.head, ss._2.head), (ss._1.tail, ss._2.tail)))
  }

  //unfold[A,S](z:S)(f: S => Option[(A,S)]): Stream[A]
  println(mapUnfold[Int,Int](Stream(1,2,3,4))(_ + 2).take(10).toList)
  println(mapUnfold2[Int,Int](Stream(1,2,3,4))(_ + 2).take(10).toList)
  println(takeUnfold[Int](Stream(1,2,3,4), 3).toList)
  println(takeUnfold2[Int](Stream(1,2,3,4), 3).toList)
  println(takeWhileUnfold[Int](Stream(1,2,3,4))(_ < 4).toList)
  println(zipWithUnfold(Stream(1,2,3,4), Stream(5,6,7,8))((a,b) => a + b).take(10).toList)
}

//import com.mimu.fpinscala.ch5.InfiniteStreams._
