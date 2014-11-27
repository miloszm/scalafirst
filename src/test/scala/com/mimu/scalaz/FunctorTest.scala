package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._

import std.option._

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

  @Test
  def testFunctor():Unit = {

    val countVowels: String => Int = _.filter(Set('a','e','i','o','u')(_)).size

    val functor = Functor[Option]

    val show = functor.map(Some("lea harp"))(countVowels)

    println(show)

    assertEquals(Some(3), show)

  }


}
