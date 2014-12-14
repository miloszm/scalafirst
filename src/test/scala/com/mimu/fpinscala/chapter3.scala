package com.mimu.fpinscala

import org.junit.Test
import org.junit.Assert._

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
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => Nil
      case x::Nil => List(buf.toList: _*)
      case x::xs => buf += x; go(xs)
    }
    go(l)
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
  @tailrec
  private def foldLeft[A,B](as: List[A], z:B)(f: (B,A) => B): B = as match {
      case Nil => z
      case x::xs => foldLeft(xs,f(z,x))(f)
  }

  /**
   * reverse using fold - exercise 3.12
   */
  private def reverseUsingFold[A](as: List[A]):List[A] = {
    // B is List[A]
    // A is A
    // z is Nil
    // (f: (B,A) => B) is a :: b
    foldLeft[A,List[A]](as, Nil:List[A])((x:List[A], y:A) => y :: x )
  }


  /**
   * reverse - exercise 3.12
   */
  private def reverse2[A](as: List[A]):List[A] = {
    @tailrec
    def doReverse(as:List[A], acc:List[A]):List[A] = as match {
      case Nil => acc
      case x::xs => doReverse(xs, x::acc)
    }
    doReverse(as, Nil)
  }

  /**
   * 3.13
   * foldLeft in terms of foldRight
   */
  private def foldLeftR[A,B](as: List[A], z:B)(f: (B,A) => B): B = {
    foldRight(as.reverse, z)((a,b)=>f(b,a))
  }

  /**
   * 3.13
   * foldRight in terms of foldLeft
   */
  private def foldRightL[A,B](as: List[A], z:B)(f: (A,B) => B): B = {
    foldLeft(as.reverse, z)((a,b)=>f(b,a))
  }

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
//foldLeft(List(1,2,3), 0)(_ + _)
//    foldLeft(List(1,2,3), (b:Int => b))

  /**
   * this is hard - our B is now (Int)=>Int
   * our z is (b:Int) => b
   * our f is (g,a) => b => g(a + b)
   * so our result is a function - at each step (g and a) are converted to a function b => g(a + b)
   * so one argument gets applied and function is composed with the next one
   * at the end such long-composed function with 1 missing argument remains
   * if we apply z to it, we get the result
   */
  @Test
  def testFoldRightViaFoldLeft_1(): Unit ={
    val f = foldLeft[Int,(Int)=>Int](List(1,2,3), ((b:Int) => b))((g,a) => b => g(a + b))
    println(f)
    println(foldLeft[Int,(Int)=>Int](List(1,2,3), ((b:Int) => b))((g,a) => b => g(a + b))(0))
  }

  /**
   * 3.14
   */
  private def append2[A](l:List[A], a:A):List[A] = {
    foldRight(l, List(a))((a,b) => a :: b)
  }

  @Test
  def testAppend2(): Unit ={
    assertEquals(List(1,2,3,4), append2(List(1,2,3), 4))
  }

  /**
   * 3.15
   * concatenates a list of lists into a single list
   */
  private def simpleFlatten(l:List[List[Int]]): List[Int] = l match {
    case Nil => Nil
    case x::xs => x ::: simpleFlatten(xs)
  }

  @Test
  def testSimpleFlatten(): Unit = {
    assertEquals(List(1,2,3,4,5,6,7,8,9), simpleFlatten(List(List(1,2,3), List(4,5,6), List(7,8,9))))
  }

  /**
   * 3.16
   */
  private def add1ToEachElem(l:List[Int]):List[Int] = {
    @tailrec
    def doAdd1ToEachElem(l:List[Int], acc:List[Int]):List[Int] = l match {
      case Nil => acc
      case x::xs => doAdd1ToEachElem(xs, (x+1) :: acc)
    }
    doAdd1ToEachElem(l, Nil)
  }

  /**
   * 3.18
   */
  private def map2[A,B](l:List[A])(f:A => B):List[B] = {
    @tailrec
    def doMap2(l:List[A], acc:List[B]):List[B] = l match {
      case Nil => acc
      case x::xs => doMap2(xs, f(x) :: acc)
    }
    doMap2(l, Nil)
  }

  /**
   * 3.19
   */
  private def filter[A](l:List[A])(f:A => Boolean):List[A] = {
    @tailrec
    def doFilter(l:List[A], acc:List[A]):List[A] = l match {
      case Nil => acc
      case x::xs if f(x) => doFilter(xs, x :: acc)
      case x::xs => doFilter(xs, acc)
    }
    doFilter(l, Nil)
  }

  /**
   * 3.16 b
   */
  private def add1ToEachElem2(l:List[Int]):List[Int] = l match {
      case Nil => Nil
      case x::xs => (x + 1) :: add1ToEachElem2(xs)
  }

  @Test
  def testChapter3_ex_1_10(): Unit ={
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
    println("fold right length of list(2) ", foldRight(List(1,2,3,4), 0)((x,_) => x + 1))  // exercise 3.9(2)
  }

  @Test
  def testChapter3_ex_11_19(): Unit ={
    assertEquals(10, foldLeft(List(1,2,3,4), 0)(_ + _)) // exercise 3.11 sum
    assertEquals(24, foldLeft(List(1,2,3,4), 1)(_ * _))  // exercise 3.11 product
    assertEquals(4, foldLeft(List(1,2,3,4), 0)((x,_) => x + 1))  // exercise 3.11 length of list(2)
    assertEquals(List(4, 3, 2, 1), reverse2(List(1,2,3,4)))  // exercise 3.12
    assertEquals(List(4, 3, 2, 1), reverseUsingFold(List(1,2,3,4)))  // exercise 3.12

    /**
     * 3.13 foldLeft via foldRight
     */
    assertEquals(10, foldLeftR(List(1,2,3,4), 0)(_ + _))
    assertEquals(24, foldLeftR(List(1,2,3,4), 1)(_ * _))
    assertEquals(4, foldLeftR(List(1,2,3,4), 0)((x,_) => x + 1))

    /**
     * 3.13 foldRight via foldLeft
     */
    assertEquals(10, foldRightL(List(1,2,3,4), 0)(_ + _))
    assertEquals(24, foldRightL(List(1,2,3,4), 1)(_ * _))
    assertEquals(4, foldRightL(List(1,2,3,4), 0)((_,y) => y + 1))
    assertEquals(4, foldRightViaFoldLeft_1(List(1,2,3,4), 0)((_,y) => y + 1))

    assertEquals(List(1,2,3,4,5,6,7,8,9), simpleFlatten(List(List(1,2,3), List(4,5,6), List(7,8,9))))  // exercise 3.15
    assertEquals(List(1,2,3,4), append2(List(1,2,3), 4))
    assertEquals(List(5, 4, 3, 2), add1ToEachElem((List(1,2,3,4))))  // exercise 3.16
    assertEquals(List(2, 3, 4, 5), add1ToEachElem2((List(1,2,3,4))))  // exercise 3.16
    assertEquals(List(2,1), filter((List(1,2,3,4)))(_ < 3))  // exercise 3.19 TODO

  }

  /**
   * 3.20
   */
  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {

    foldLeft[A,List[B]](as, Nil:List[B])((x:List[B], y:A) => x ::: f(y) )

  }

  /**
   * 3.21
   * use flatMap to implement filter
   */
  def filter2[A](l:List[A])(f:A => Boolean):List[A] = {

    flatMap2[A,A](l)((a:A) => if (f(a)) List(a) else Nil): List[A]

  }

  /**
   * 3.22 and 3.23
   */
  def zipWith2[A](l1:List[A], l2:List[A])(f:(A,A)=>A):List[A] = (l1,l2) match {

    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case ((x:A)::xs, (y:A)::ys) => f(x,y) :: zipWith2(xs,ys)(f)

  }

  @Test
  def testChapter4_ex_20_29(): Unit ={

    assertEquals(List(1,1,2,2,3,3), flatMap2(List(1,2,3))(x => List(x,x)))
    assertEquals(List(1,2,3,4,5,6), filter2(List(1,2,3,4,5,6,7,8,9))(_ < 7))
    assertEquals(List(5,7,9),zipWith2[Int](List(1,2,3),List(4,5,6))(_ + _))
    assertEquals(List("abc","def","ghi"),zipWith2[String](List("ab","de","gh"),List("c","f","i"))(_ + _))

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
