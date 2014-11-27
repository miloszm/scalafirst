package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._

/**
 * Created by mm.
 *
 * Monads provide a solution to the following problem:
 * if we have a value with contaxt, m a, how do we apply it to a function
 * that takes normal a and returns a value with a context
 *
 */
class MonadTest {

  /**
   * simple monad test - no implicits
   */
  @Test
  def testMonadNoImplicits():Unit = {

    val monad = Monad[Option](createMonadForOption)

    println(monad)

    val result = monad.bind(Some("abc"))({s:String => Some(s + s)})
    println(result)
    assertEquals(Some("abcabc"), result)

  }

  /**
   * simple monad test - with implicits
   */
  @Test
  def testMonadWithImplicits():Unit = {

    import std.option._

    val monad = Monad[Option]

    println(monad)

    val result = monad.bind(Some("abc"))({s:String => Some(s + s)})
    println(result)
    assertEquals(Some("abcabc"), result)

  }

  /**
   * creates monad for option
   * in order to avoid using implicits from scalaz
   * when creating Monad[Option], second argument is implicit
   * and the result of this method may be used instead
   * this gives us self-containment of this test (for pedagogical reasons)
   */
  def createMonadForOption: Monad[Option] = {

    new Monad[Option] {
      /**
       * for comparison: point[A](a: => A): Option[A]   //// from Applicative
       */
      override def point[A](a: => A): Option[A] = {
        Some(a)
      }

      /**
       * for comparison: ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B]   //// from Applicative
       */
      override def bind[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = {
        fa match {
          case Some(x) => f(x)
          case None => None
        }
      }
    }
  }

  /**
   * string list monad test - with implicits
   */
  @Test
  def testStringListMonadWithImplicits():Unit = {

    import std.list._

    val monad = Monad[List]

    println(monad)

    val result = monad.bind(List("abc"))({s:String => List(s + s)})
    println(result)
    assertEquals(List("abcabc"), result)

  }


  /**
   * string list monad test - without implicits
   */
  @Test
  def testStringListMonadWithoutImplicits():Unit = {

    val monad = Monad[List](createMonadForList)

    println(monad)

    val result = monad.bind(List("abc"))({s:String => List(s + s)})
    println(result)
    assertEquals(List("abcabc"), result)

  }


  /**
   * creates monad for list
   * in order to avoid using implicits from scalaz
   * when creating Monad[List], second argument is implicit
   * and the result of this method may be used instead
   * this gives us self-containment of this test (for pedagogical reasons)
   */
  def createMonadForList: Monad[List] = {

    new Monad[List] {
      override def point[A](a: => A): List[A] = {
        List(a)
      }

      override def bind[A, B](fa: List[A])(f: (A) => List[B]): List[B] = {
        fa match {
          case x::xs => f(x)
          case Nil => Nil
        }
      }
    }
  }


}
