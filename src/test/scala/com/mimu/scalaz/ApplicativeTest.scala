package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._

/**
 * Created by mm.
 *
 * Applicative Functor
 *
 *
 *
 * NOTE - we are not using scalaz implicits here but rather we
 * provide our own implementations of Functor[Option] and Application[Option]
 *
 *
 */
class ApplicativeTest {

  /**
   * let's test option functor with function
   *
   * map[A, B](fa: F[A])(f: A => B): F[B]
   *
   * our f is a function which takes two arguments
   * our B is a partially applied function
   * can we do that in Scala ?
   */
  @Test
  def testFunctorWithPartiallyAppliedFunction():Unit = {

    val functor = Functor[Option](createFunctorForOption)

    def fun(x:String)(y:String):String = {
      x + y
    }

    /**
     * input to our mapping is a 2 arg function which will be partially applied
     */
    val result = functor.map(Some("abc"))(fun)
    println(s"result of mapping is: $result")

    result match {
      case Some(x) => println(s"result of mapping inside Some is: $x")
      case _ => fail
    }

    /**
     * now we can extract the content of some - it will be a partially
     * applied function, and apply the remaining parameter to it
     * it should produce the desired result of concatenating "abc" and "def"
     */
    println(result.getOrElse({s:String => "zzz"}).apply("def"))

  }


  /**
   * summary:
   *
   */
  @Test
  def testApplicativeFunctor():Unit = {

    val appFunctor = Applicative[Option](createApplicativeFunctorForOption)

    def fun(x:String):String = {
      "Scala " + x
    }

    val result = appFunctor.ap(Some("Rules"))(Some(fun _)) // underscore needed to treat fun as a partially applied function
    println(result)
    assertEquals(Some("Scala Rules"),result)

  }

  /**
   * summary:
   *
   */
  @Test
  def testApplicativeFunctorChangingType():Unit = {

    val appFunctor = Applicative[Option](createApplicativeFunctorForOption)

    def fun(x:String):Int = {
      x.length
    }

    val result = appFunctor.ap(Some("ScalaRules"))(Some(fun _)) // underscore needed to treat fun as a partially applied function
    println(result)
    assertEquals(Some(10),result)

  }

  /**
   * creates applicative functor for option
   * in order to avoid using implicits from scalaz
   * when creating Applicative[Option], second argument is implicit
   * and the result of this method may be used instead
   * this gives us self-containment of this test (for pedagogical reasons)
   */
  def createApplicativeFunctorForOption: Applicative[Option] with Object {def point[A](a: => A): Option[A]; def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B]} = {

    new Applicative[Option] {
      override def point[A](a: => A): Option[A] = {
        Some(a)
      }

      override def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B] = {
        for (
          a <- fa;
          ff <- f
        )
        yield ff(a)
      }
    }
  }

  /**
   * creates functor for option
   * in order to avoid using implicits from scalaz
   * when creating Functor[Option], second argument is implicit
   * and the result of this method may be used instead
   * this gives us self-containment of this test (for pedagogical reasons)
   */
  def createFunctorForOption: Functor[Option] with Object {def map[A, B](fa: Option[A])(f: (A) => B): Option[B]} = {
    new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
        for (e <- fa) yield f(e)
      }
    }
  }


}
