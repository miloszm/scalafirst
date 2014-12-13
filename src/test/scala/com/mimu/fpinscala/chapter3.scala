package com.mimu.fpinscala

import org.junit.Test

import scala.annotation.tailrec

/**
 * Created by mm.
 */
class chapter3 {

  /**
   * 3.2
   */
  def tail[A](l:List[A]): List[A] = l match {
      case Nil => Nil
      case x :: xs => xs
  }

  /**
   * 3.3
   */
  def setHead[A](newHead:A, l:List[A]): List[A] = l match {
      case Nil => List(newHead)
      case x :: xs => newHead :: xs
  }

  /**
   * 3.4
   */
  def drop[A](l:List[A], n:Int): List[A] = l match {
    case _ if (n == 0) => l
    case x::xs => drop(xs, n-1)
  }

  /**
   * 3.5
   */
  def dropWhile[A](l:List[A], f: A => Boolean): List[A] = l match {
    case x::xs if f(x) => dropWhile(xs, f)
    case _ => l
  }

  /**
   * 3.5 (a) - the main reason for grouping the arguments this way is to assist
   * with type inference
   */
  def dropWhile2[A](l:List[A])(f: A => Boolean): List[A] = l match {
    case x::xs if f(x) => dropWhile2(xs)(f)
    case _ => l
  }

  /**
   * 3.6
   */
  def init[A](l:List[A]): List[A] = {
    tail(l.reverse).reverse
  }


  /**
   * fold right
   */
  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = as match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }


  /**
   * 3.7
   * fold right with short-circuit
   */
  def foldRight2[A,B](as: List[A], z:B)(f: (A,B) => B): B = as match {
    case Nil => z
    case x::xs if x == z => z
    case x::xs if x != z => f(x, foldRight2(xs,z)(f))
  }


  /**
   * fold left for exercise 3.10
   */
  private def foldLeft[A,B](as: List[A], z:B)(f: (B,A) => B): B = {
    @tailrec
    def doFoldLeft(as: List[A], z:B, acc:B):B = as match {
      case Nil => acc
      case x::xs => doFoldLeft(xs,z,f(acc,x))
    }
    doFoldLeft(as.reverse, z, z)
  }


  @Test
  def testChapter3(): Unit ={
    println(tail(List(1,2,3,4)))
    println(setHead(5, List(1,2,3,4)))
    println(setHead(5, List()))
    println("drop " + drop(List(1,2,3,4,5), 3))
    println(dropWhile(List(1,2,3,4,5), (x:Int) => (x < 4)))
    println(dropWhile(List(1,2,3,4,5), ((_:Int) < 4)))
    println(dropWhile2(List(1,2,3,4,5))(_ < 4))
    println(foldRight(List(1,2,3,4), 0)(_ + _))
    println(foldRight2(List(1,2,0,4), 0)(_ + _))
    println(foldRight(List(1,2,3,4), Nil:List[Int])(_ :: _))  // exercise 3.8
    println(foldRight(List(1,2,3,4) map ((x)=>1), 0)((x,y) => x + y))  // exercise 3.9
  }

  /**
   * 3.10
   * convince yourself that foldRight will result in a StackOverflowError for large lists
   * Indeed - array larger than 5400 gives StackOverflowError
   */
  @Test
  def testFoldRightStackOverflow(): Unit ={
    val a = Array.fill(5300)(1)
    println(foldRight[Int,Int](a.toList, 0)(_ + _))
  }

  /**
   * 3.10
   * convince yourself that foldLeft won't result in a StackOverflowError for large lists
   * Indeed - it works event for million elements !!
   */
  @Test
  def testFoldLeftNoStackOverflow(): Unit ={
    val a = Array.fill(1000000)(1)
    println(foldLeft[Int,Int](a.toList, 0)(_ + _))
  }

}
