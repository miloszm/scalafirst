package com.mimu.scalaz

import org.junit.Test
import org.junit.Assert._

import scalaz._
import Scalaz._


/**
 *
 *
 * code from: http://eed3si9n.com/learning-scalaz/Walk+the+line.html
 */
class WalkTheLineMonadTest {

  type Birds = Int

  case class Pole(left: Birds, right: Birds){
    def landLeft(n: Birds): Option[Pole] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none
    def landRight(n: Birds): Option[Pole] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none
    def banana: Option[Pole] = none
  }

  /**
   * walk the line monad tests
   */
  @Test
  def testWalkTheLineMonad():Unit = {

    val v1 = Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)}

    assertEquals(Some(Pole(2,4)), v1)

    val v2 = Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)} >>= {_.landRight(2)}

    assertEquals(None, v2)

    val v3 = Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)} >>= {_.banana}

    assertEquals(None, v3)

    val v4 = (Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)}) >> (none: Option[Pole]) >>= {_.landRight(1)}

    assertEquals(None, v4)

    /**
     * Scala equivalent of Haskell's do
     */
    val v5 = for {
      x <- 3.some
      y <- "!".some
    } yield (x.shows + y)

    assertEquals(Some("3!"), v5)

    /**
     * Scala equivalent of Haskell's do
     */
    val v6 = for {
      start <- Monad[Option].point(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield third

    assertEquals(Some(Pole(3,2)), v6)

    /**
     * banana
     */
    val v7 = for {
      start <- Monad[Option].point(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
      _ <- (none: Option[Pole])
      third <- second.landLeft(1)
    } yield third

    assertEquals(None, v7)

  }

  /**
   * pattern matching in do/for
   */
  @Test
  def testPatternMatchingInDoFor(): Unit = {

    assertEquals(Some('h'), (
      for {
        (x :: xs) <- "hello".toList.some
      } yield x))

    assertEquals(None, (
      for {
        (x :: xs) <- "".toList.some
      } yield x))

  }

}
