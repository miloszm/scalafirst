package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._

/**
 * Created by mm.
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


}
