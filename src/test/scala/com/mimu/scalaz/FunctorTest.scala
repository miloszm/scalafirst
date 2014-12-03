package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

//import std.option._ /* why do we need this ? we need it for implicit Functor implementations for options*/
//import std.list._   /* why do we need this ? */

/**
 * Created by mm.
 *
 * Functor - it kind of applies the function to the element inside the box.
 * Box could really mean something like "computational context".
 *
 * Some type that accepts another type is a functor, for example List[_] or Option[_].
 *
 *   * The Functor category involves a single operation, named `map`:
 *
 * def map[A, B](fa: F[A])(f: A => B): F[B]
 *
 * This method takes a Function from A => B and turns an F[A] into an F[B]

 *
 */
class FunctorTest {

  /**
   * summary:
   *
   * def map[A, B](fa: F[A])(f: A => B): F[B]
   *
   * our F[A] here was Option[String]
   * our A => B here was countVowels: String => Int
   * our F[B} here was Option[Int]
   */
  @Test
  def testFunctorWithOption():Unit = {

    val countVowels: String => Int = _.filter(Set('a','e','i','o','u')(_)).size

    val functor = Functor[Option]

    val showSome:Option[Int] = functor.map(Some("lea harp"))(countVowels)
    println(showSome)
    assertEquals(Some(3), showSome)

    val showNone:Option[Int] = functor.map(None)(countVowels)
    println(showNone)
    assertEquals(None, showNone)

  }

  /**
   * Error:(57, 26) could not find implicit value for parameter F: scalaz.Functor[List]
   * val functor = Functor[List]
   */
  @Test
  def testFunctorWithList():Unit = {

    val countLettersA: String => Int = _.filter(_ == 'a').size

    /**
     * this implicit is needed for Functor creation, if it were missing
     * we'd need to import std.list._
     * Traverse[List] extends Functor[List] so it is an implicit implementation of Functor[List]
     */
//    implicit val listInstance = new scalaz.Traverse[List] {
//      override def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]) = {
//        DList.fromList(l).foldr(F.point(List[B]())) {
//          (a, fbs) => F.apply2(f(a), fbs)(_ :: _)
//        }
//      }
//    }
    /**
     * let's try to provide our own implicit Functor implementation for lists
     */
    implicit val listFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: (A) => B): List[B] = {
        for (e <- fa) yield f(e)
      }
    }

    val functor = Functor[List]
    /**
     * if we do not have implicit, we can provide it explicitly
     */
    //val functor = Functor[List](listFunctor)

    val show:List[Int] = functor.map(List("lea harp", "abracadabra"))(countLettersA)
    println(show)
    assertEquals(List(2,5), show)

    val showNil:List[Int] = functor.map(List())(countLettersA)
    println(showNil)
    assertEquals(List(), showNil)

  }

  /**
   * laws
   */
  @Test
  def functorLaws(): Unit ={

    import Scalaz._


    List(1, 2, 3) map {identity} assert_=== List(1, 2, 3)

    (List(1, 2, 3) map {{(_: Int) * 3} map {(_: Int) + 1}}) assert_=== (List(1, 2, 3) map {(_: Int) * 3} map {(_: Int) + 1})


    /**
     * I can't make the following to compile
     */
//    class FakeFunctor[F[_]] {
//
//      def mapx[A, B](fa: F[A])(f: A => B): F[B] = for (e <- fa) yield f(e)
//
//      def checkIdentity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(mapx(fa)((x:A) => x), fa)
//
//    }
//
//    val fakeOptionFunctor = new FakeFunctor[List]
//    fakeOptionFunctor.checkIdentity(List(3:Int)) assert_=== true


  }


}
