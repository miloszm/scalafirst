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
 *
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

    val optionFunctor = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
        for (e <- fa) yield f(e)
      }
    }

    val functor = Functor[Option](optionFunctor)

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

    val optionApplicativeFunctor = new Applicative[Option] {
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

    val appFunctor = Applicative[Option](optionApplicativeFunctor)

    def fun(x:String):String = {
      "Scala " + x
    }

//    val result = appFunctor.map(Some("Scala"))(fun)
//    println(result)

    val result = appFunctor.ap(Some("Rules"))(Some(fun _)) // underscore needed to treat fun as a partially applied function
    println(result)

  }

}
