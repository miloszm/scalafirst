package com.mimu.scalaz

import org.junit.Test
import org.junit.Assert._

import scalaz._
import Scalaz._

/**
 * Created by mm.
 */
class MonoidTest {

  @Test
  def testMonoidListStringConcatenation():Unit = {

    class MmListMonoid extends Monoid[List[String]] {

      override def zero: List[String] = List()

      override def append(f1: List[String], f2: => List[String]): List[String] =  f1 ::: f2

    }

    val lm = new MmListMonoid()

    lm.append(List("abc"), List("def")) match {
      case x::y::xs => if (x != "abc" || y != "def") fail
      case _ => fail
    }

    lm.append(List("abc"), lm.zero) match {
      case x::xs => if (x != "abc" || xs != Nil) fail
      case _ => fail
    }

  }

  @Test
  def testMonoidIntAddition():Unit = {

    class MmIntMonoid extends Monoid[Int] {

      override def zero: Int = 0

      override def append(a1: Int, a2: => Int): Int =  a1 + a2

    }

    val lm = new MmIntMonoid()

    assertEquals(3, lm.append(1, 2))

    assertEquals(2, lm.append(2, lm.zero))


    import Scalaz._

    assertEquals(3, 1 |+| 2)

    assertEquals(2, 2 |+| lm.zero)
  }

  @Test
  def testMonoidIntMultiplication():Unit = {

    class MmIntMonoid extends Monoid[Int] {

      override def zero: Int = 1

      override def append(a1: Int, a2: => Int): Int =  a1 * a2

    }

    val lm = new MmIntMonoid()

    assertEquals(168, lm.append(12, 14))

    assertEquals(14, lm.append(14, lm.zero))

  }

  @Test
  def testMonoidBooleanAnd():Unit = {

    class MmIntMonoid extends Monoid[Boolean] {

      override def zero: Boolean = true

      override def append(a1: Boolean, a2: => Boolean): Boolean =  a1 && a2

    }

    val lm = new MmIntMonoid()

    assertEquals(false, lm.append(false, false))
    assertEquals(false, lm.append(true , false))
    assertEquals(false, lm.append(false, true ))
    assertEquals(true , lm.append(true , true ))

    assertEquals(false, lm.append(false, lm.zero))
    assertEquals(true , lm.append(true , lm.zero))

  }

  @Test
  def testMonoidBooleanOr():Unit = {

    class MmIntMonoid extends Monoid[Boolean] {

      override def zero: Boolean = false

      override def append(a1: Boolean, a2: => Boolean): Boolean =  a1 || a2

    }

    val lm = new MmIntMonoid()

    assertEquals(false, lm.append(false, false))
    assertEquals(true , lm.append(true , false))
    assertEquals(true , lm.append(false, true ))
    assertEquals(true , lm.append(true , true ))

    assertEquals(false, lm.append(false, lm.zero))
    assertEquals(true , lm.append(true , lm.zero))

  }

  @Test
  def testMonoidOptionStringConcat():Unit = {

    class MmOptionMonoid extends Monoid[Option[String]] {

      override def zero: Option[String] = None

      /**
       * note - this is monadial behaviour, not monoidal
       */
      override def append(a1: Option[String], a2: => Option[String]): Option[String] = {
        for (
          s1 <- a1;
          s2 <- a2
        )
        yield (s1 + s2)
      }

    }

    val lm = new MmOptionMonoid()

    assertEquals(Some("aabb"), lm.append(Some("aa"), Some("bb")))

    /**
     * note - this is monadial behaviour, not monoidal
     * for monoids we should have assertEquals(Some("aa").... rather than assertEquals(None....
     */
    assertEquals(None, lm.append(Some("aa"), lm.zero))
    assertEquals(None, lm.append(lm.zero, Some("bb")))

    assertEquals(Some("aabbcc"), lm.append(lm.append(Some("aa"), Some("bb")), Some("cc")))
    assertEquals(Some("aabbcc"), lm.append(Some("aa"), lm.append((Some("bb")), Some("cc"))))
  }

  /**
   * misc Monoid tests
   */
  @Test
  def miscMonoidTests(): Unit ={

    /**
     * thanks to Scalaz import normal lists and strings are elevated to be Monoids
     * and support Monoid append operation
     */
    println(List(1, 2, 3) |+| List(4, 5, 6))
    println("one" |+| "two")


    /**
     * tagging and zero
     */
    assertEquals(1, Monoid[Int @@ Tags.Multiplication].zero)
    assertEquals(0, Monoid[Int].zero)
    assertEquals("", Monoid[String].zero)
    assertEquals(List(), Monoid[List[String]].zero)
    assertEquals(false, Monoid[Boolean @@ Tags.Disjunction].zero)
    assertEquals(true, Monoid[Boolean @@ Tags.Conjunction].zero)
    assertEquals(Ordering.EQ, Monoid[Ordering].zero)

    /**
     * tagging and Monoid append
     */
    assertEquals(50, Tags.Multiplication(10) |+| Tags.Multiplication(5))
    assertEquals(15, 10 |+| 5)
    assertEquals(true, Tags.Disjunction(true) |+| Tags.Disjunction(false))
    assertEquals(false, Tags.Conjunction(true) |+| Tags.Conjunction(false))
    assertEquals(Ordering.LT, (Ordering.LT: Ordering) |+| Monoid[Ordering].zero)
    assertEquals(Ordering.LT, (Ordering.LT: Ordering) |+| (Ordering.GT: Ordering))
    assertEquals(Ordering.GT, (Ordering.GT: Ordering) |+| (Ordering.LT: Ordering))

    /**
     * example from LearnYouAHaskell - comparing according to length and if equal, lexicographically
     */
    def lengthCompare(lhs: String, rhs: String): Ordering =
      (lhs.length ?|? rhs.length) |+| (lhs ?|? rhs)

    assertEquals(Ordering.LT, lengthCompare("zzz", "aaaa"))
    assertEquals(Ordering.GT, lengthCompare("aab", "aaa"))

  }


}
