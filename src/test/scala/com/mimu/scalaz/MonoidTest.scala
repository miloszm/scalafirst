package com.mimu.scalaz

import org.junit.Test
import org.junit.Assert._

import scalaz._

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

}
