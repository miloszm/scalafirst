package com.mimu.implicits

/**
 * Created by mm.
 *
 * Tests for section 21.1 Implicit Conversions
 *
 */

import com.mimu.Fraction
import org.junit.Test


class FractionTest {

  @Test
  def testFractionMultiplication() = {
    val f1:Fraction = Fraction(1,2)
    val f2:Fraction = Fraction(3,4)

    println(f1*f2)

    println(2 * f2)

  }


}
