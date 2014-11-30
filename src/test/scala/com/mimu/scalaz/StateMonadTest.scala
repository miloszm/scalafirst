package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 * from: http://eed3si9n.com/learning-scalaz/State.html
 */
class StateMonadTest {

  /**
   * state without state monad
   */
  @Test
  def testState():Unit = {

    type Stack = List[Int]

    def pop(stack: Stack): (Int, Stack) = stack match {
      case x :: xs => (x, xs)
    }

    def push(a: Int, stack: Stack): (Unit, Stack) = ((), a :: stack)

    def stackManip(stack: Stack): (Int, Stack) = {
      val (_, newStack1) = push(3, stack)
      val b@(a1, newStack2) = pop(newStack1)
      println(b)
      val c@(a2, newStack3) = pop(newStack2)
      println(c)
      c
    }

    println(stackManip(List(5, 8, 2, 1)))
  }

  /**
   * state monad
   */
  @Test
  def testStateMonad():Unit = {

    type Stack = List[Int]

    /**
     * this works thanks to state's apply
     */
    val pop = State[Stack, Int] {
      case x :: xs => (xs, x)
    }

    /**
     * this works thanks to state's apply
     */
    def push(a: Int) = State[Stack, Unit] {
      case xs => (a :: xs, ())
    }

    /**
     * cool
     *
     * we can monadically chain each operations using for syntax without manually passing around the Stack values
     */
    def stackManip: State[Stack, Int] = for {
      _ <- push(3)
      a <- pop
      b <- pop
    } yield(b)

    println(stackManip(List(5, 8, 2, 1)))

  }

  /**
   * stacky stack test
   */
  @Test
  def testStackyStack():Unit = {

    type Stack = List[Int]

    def stackyStack: State[Stack, Unit] = for {
      stackNow <- get
      r <- if (stackNow === List(1, 2, 3)) put(List(8, 3, 1))
      else put(List(9, 2, 1))
    } yield r

    println(stackyStack(List(1, 2, 3)))
    println(stackyStack(List(1, 2, 4)))

  }

  /**
   * pop and push in terms of get and put
   */
  def popAndPush(): Unit ={

    type Stack = List[Int]

    val pop: State[Stack, Int] = for {
      s <- get[Stack]
      (x :: xs) = s
      _ <- put(xs)
    } yield x

    def push(x: Int): State[Stack, Unit] = for {
      xs <- get[Stack]
      r <- put(x :: xs)
    } yield r

  }


}
