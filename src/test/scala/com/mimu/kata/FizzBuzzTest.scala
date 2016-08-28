package com.mimu.kata

import org.scalatest.{FlatSpec, Matchers}

class FizzBuzzTest extends FlatSpec with Matchers {

  "FizzBuzz generator" should "produce Fizz for multiples of 3" in {
    FizzBuzz.generate(9) shouldBe "Fizz"
  }

  "FizzBuzz generator" should "produce Fizz for if it contains 3" in {
    FizzBuzz.generate(31) shouldBe "Fizz"
  }

  "FizzBuzz generator" should "produce Buzz for multiples of 5" in {
    FizzBuzz.generate(10) shouldBe "Buzz"
  }

  "FizzBuzz generator" should "produce Buzz for if it contains 3" in {
    FizzBuzz.generate(25) shouldBe "Buzz"
  }

  "FizzBuzz generator" should "produce FizzBuzz for if it is a multple of 3 and 5" in {
    FizzBuzz.generate(15) shouldBe "FizzBuzz"
  }

  "FizzBuzz generator" should "produce FizzBuzz for if it contains 3 and 5 in this order" in {
    FizzBuzz.generate(35) shouldBe "FizzBuzz"
  }

  "FizzBuzz generator" should "produce FizzBuzz for if it contains 5 and 3 in this order" in {
    FizzBuzz.generate(53) shouldBe "FizzBuzz"
  }

  "FizzBuzz generator" should "produce n as string if not multiple of 3 and 5 and does not contain 3 or 5" in {
    FizzBuzz.generate(62) shouldBe "62"
  }
}
