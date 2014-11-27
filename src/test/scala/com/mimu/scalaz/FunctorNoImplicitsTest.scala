package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
//import scalaz.std.option._ /* why do we need this ? we need it for implicit Functor implementations for options*/

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
class FunctorNoImplicitsTest {

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
  def testFunctorWithOptionNoImplicits():Unit = {

    val countVowels: String => Int = _.filter(Set('a','e','i','o','u')(_)).size

    /**
     * let's try to provide our own implicit Functor implementation for options
     */
    /*implicit*/ val optionFunctor = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
        for (e <- fa) yield f(e)
      }
    }

    //val functor = Functor[Option]
    val functor = Functor[Option](optionFunctor)

    val showSome:Option[Int] = functor.map(Some("lea harp"))(countVowels)
    println(showSome)
    assertEquals(Some(3), showSome)


    val showNone:Option[Int] = functor.map(None)(countVowels)
    println(showNone)
    assertEquals(None, showNone)


  }

}
