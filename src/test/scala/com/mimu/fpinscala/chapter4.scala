package com.mimu.fpinscala

import org.junit.Assert._
import org.junit.Test
import chapter4test.{OptionM,SomeM,NoneM}

/**
 * Created by mm.
 */



class chapter4 {

  @Test
  def testOptionMap(): Unit ={
    assertEquals(SomeM(6), SomeM(3) map (_*2))
    val niente:OptionM[Int] = NoneM
    assertEquals(NoneM, niente map (_*2))
  }

  @Test
  def testOptionGetOrElse(): Unit ={
    assertEquals(3, SomeM(3).getOrElse(5))
    val niente:OptionM[Int] = NoneM
    assertEquals(5, niente getOrElse (5))
  }

  @Test
  def testFlatMap(): Unit ={
    assertEquals(SomeM(3), SomeM(3).flatMap((x) => if (x == 3) SomeM(3) else NoneM))
    assertEquals(NoneM, SomeM(4).flatMap((x) => if (x == 3) SomeM(3) else NoneM))
    val niente:OptionM[Int] = NoneM
    assertEquals(NoneM, niente flatMap ((x) => if (x == 3) SomeM(3) else NoneM))
  }

  @Test
  def testOrElse(): Unit ={
    assertEquals(SomeM(3), SomeM(3) orElse SomeM(4))
    val niente:OptionM[Int] = NoneM
    assertEquals(SomeM(4), niente orElse SomeM(4))
  }

  @Test
  def testFilter(): Unit ={
    assertEquals(SomeM(3), SomeM(3) filter (_ == 3))
    assertEquals(NoneM, SomeM(3) filter (_ == 4))
    val niente:OptionM[Int] = NoneM
    assertEquals(NoneM, niente filter (_ == 5))

  }

  @Test
  def testOptionWithMap(): Unit ={

    case class PersonM(name:String, dept:String)
    def lookup(n:String):OptionM[PersonM] = if (n.length > 2) SomeM(PersonM("john","cs")) else NoneM

    assertEquals(SomeM("cs"), lookup("abc") map (_.dept))
    assertEquals(NoneM, lookup("ab") map (_.dept))

    assertEquals("defdept", lookup("ab") map (_.dept) getOrElse("defdept"))
  }

  /**
   * from book's code
   */
  def mean(xs: Seq[Double]): OptionM[Double] =
    if (xs.isEmpty) NoneM
    else SomeM(xs.sum / xs.length)

  /**
   * from book's code
   */
  def variance(xs: Seq[Double]): OptionM[Double] = {
    val ff:Double => OptionM[Double] = (m:Double) => mean(xs.map(x => math.pow(x - m, 2))) // ff may fail
    mean(xs) flatMap (ff)
  }

  @Test
  def testVariance(): Unit ={
    println(variance(List(1,2,3,4,5)))
    assertEquals(SomeM(2.0),variance(List(1,2,3,4,5)))
  }

  /**
   * map turns function of type A => B into a function of type F[A] => F[B]
   */
  def lift[A,B](f: A => B): OptionM[A] => OptionM[B] = (x:OptionM[A]) => x map f
  // which is the same as
  def liftM[A,B](f: A => B): OptionM[A] => OptionM[B] = _ map f

  /**
   * 4.3
   */
  def map2[A,B,C](a:OptionM[A], b:OptionM[B])(f: (A,B) => C): OptionM[C]  = {
    for {
      aa <- a
      bb <- b
    }
    yield(f(aa,bb))

//    println (s"map2 called with $a $b")
//
//    def fff(aa:A, bb:B)(f: (A,B) => C) : C = {
//      println (s"map2 performing f for $aa $bb")
//      f(aa,bb)
//    }
//
//    val r = a flatMap (aa => b map (bb => fff(aa, bb)(f)))
//    println(s"map2 returning $r")
//    r

  }

  @Test
  def testMap2() = {
    val f = (x:Int,y:Int) => x + y
    println(map2(SomeM(3), SomeM(4))(f))
    assertEquals(SomeM(7), map2(SomeM(3), SomeM(4))(f))
  }

  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = as match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }

  /**
   * 4.4
   */
  def sequence[A](a:List[OptionM[A]]): OptionM[List[A]] = {

//    def go(a:List[OptionM[A]], acc:List[A]): List[A] = a match {
//      case Nil => acc
//      case NoneM::xs => go(xs, acc)
//      case SomeM(s)::xs => go(xs, s :: acc)
//    }
//
//    go(a, Nil) match {
//      case Nil => NoneM
//      case x => SomeM(x.reverse)
//    }

    val ff:(OptionM[A],OptionM[List[A]]) => OptionM[List[A]] = (x:OptionM[A],y:OptionM[List[A]]) => map2(x,y)((s:A,t:List[A]) => s :: t)
    //a.foldRight[OptionM[List[A]]](SomeM(Nil))(ff)
    foldRight[OptionM[A],OptionM[List[A]]](a,SomeM(Nil))(ff)

    //a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  }

  /**
   * 4.5
   * sequence in terms of traverse
   */
  def sequenceM2[A](a:List[OptionM[A]]): OptionM[List[A]] = {
    traverseM(a)((x) => x)
  }

  def traverseM[A, B](a: List[A])(f: A => OptionM[B]): OptionM[List[B]] =
    a.foldRight[OptionM[List[B]]](SomeM(Nil))((h,t) => map2(f(h),t)(_ :: _))

  @Test
  def testSequence():Unit = {
    assertEquals(SomeM(List(3, 4, 5)), sequence(List(SomeM(3), SomeM(4), SomeM(5))))

    def TryM[A](a: => A): OptionM[A] =
    try SomeM(a)
    catch { case e: Exception => NoneM }

    def parseInts(a: List[String]): OptionM[List[Int]] =
      sequenceM2(a map (ii => TryM(ii.toInt)))

    assertEquals(SomeM(List(2, 3, 5)),parseInts(List("2", "3", "5")))
    assertEquals(SomeM(List(2, 3, 5)),traverseM(List("2", "3", "5"))((ii:String) => TryM(ii.toInt)))
    assertEquals(NoneM,parseInts(List("2", "3", "5", "xxx")))
    assertEquals(NoneM,traverseM(List("2", "3", "5", "xxx"))((ii:String) => TryM(ii.toInt)))

  }

  @Test
  def testSequence2():Unit = {

    println(sequence(List(SomeM(3), SomeM(4))))

  }

}
