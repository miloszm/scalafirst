package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 * lens
 *
 * http://scalathon.org/2012/presentations/lenses.pdf
 * https://www.youtube.com/watch?v=efv0SQNde5Q - Lenses A Functional Imperative - Edward Kmett
 * http://eed3si9n.com/learning-scalaz/Lens.html
 *
 */


case class Point(x: Double, y: Double)
case class Color(r: Byte, g: Byte, b: Byte)
case class Turtle(
                   position: Point,
                   heading: Double,
                   color: Color)


class LensScalazTest {

  /**
   * scalaz lens
   */
  @Test
  def testScalazLensTurtle():Unit = {

    val turtlePosition = Lens.lensu[Turtle, Point] (
      (a, value) => a.copy(position = value),
      _.position
    )

    val pointX = Lens.lensu[Point, Double] (
      (a, value) => a.copy(x = value),
      _.x
    )

    val turtleX = turtlePosition >=> pointX  // andThen - in a sense that get turtle pos and then mess with a point inside it

    val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

    assertTrue(turtleX.get(t0) == 2.0)

    val t1 = turtleX.set(t0, 5.0)

    assertTrue(turtleX.get(t1) == 5.0)

    val t2 = turtleX.mod(_ + 1.0, t1) // will read the value, change it and set back

    assertTrue(turtleX.get(t2) == 6.0)

    val incX = turtleX =>= {_ + 1.0} // same as mod
    val t3 = incX(t2)
    assertTrue(turtleX.get(t3) == 7.0)
    println(t3)

    val incX2 = for {
      x <- turtleX %= {_ + 1.0} // %= takes fun Double => Double and returns state monad
    } yield x
    val t4 = incX2(t3)  // note incX2 returns a tuple (turtle, new value), unlike incX, which returns just turtle
    println(t4)
    assertTrue(turtleX.get(t4._1) == 8.0)


    val turtleHeading = Lens.lensu[Turtle, Double] (
      (a, value) => a.copy(heading = value),
      _.heading
    )

    val pointY = Lens.lensu[Point, Double] (
      (a, value) => a.copy(y = value),
      _.y
    )

    val turtleY = turtlePosition >=> pointY

    def forward(dist: Double) = for {
      heading <- turtleHeading
      x <- turtleX += dist * math.cos(heading)
      y <- turtleY += dist * math.sin(heading)
    } yield (x, y)

    println(t0)
    println(forward(10.0)(t0))
    println(forward(10.0) exec (t0))


    checkLawsOfLens(t0, 2.0, turtleHeading, 3.0)
    checkLawsOfLens(t0, Point(1.0,1.0), turtlePosition, Point(2.0,2.0))
    checkLawsOfLens(Point(1.0,1.0), 2.0, pointX, 3.0)
    checkLawsOfLens(Point(1.0,1.0), 2.0, pointY, 3.0)

  }


  /**
   * check laws of lens
   */
  def checkLawsOfLens[AHolder,B](aHolder:AHolder, b:B, l:Lens[AHolder,B], c:B): Unit ={
    assertTrue(l.get(l.set(aHolder, b)) == b) // I should get what I've set
    assertTrue(l.set(l.set(aHolder, b), c) == l.set(aHolder, c)) // I've flushed out whatever was there and I am getting the new value
    assertTrue(l.set(aHolder, l.get(aHolder)) == aHolder) // when I get and set immediately, I do not change anything
  }

}
