package com.mimu.fpinscala.chapter4test

import org.scalatest.{FunSpec, Matchers}

/**
  * 4.1
  */

class OptionMTest extends FunSpec with Matchers {

  describe("map of elem A with function f:A=>A") {
    it("should be some of f(elem) if elem is some") {
      SomeM(5) map (a => a*a) should be (SomeM(25))
    }
    it("should be none if elem is some") {
      (NoneM:OptionM[Int]) map (a => a*a) should be (NoneM)
    }
  }
  describe("map of elem A with function f:A=>B") {
    it("should be some of f(elem) if elem is some") {
      SomeM(5) map (_.toString) should be (SomeM("5"))
    }
    it("should be none if elem is some") {
      (NoneM:OptionM[Int]) map (_.toString) should be (NoneM)
    }
  }
  describe("flatMap of elem A with function f:A=>SomeM[A]") {
    it("should be some of f(elem) if elem is some") {
      SomeM(5) flatMap (a => SomeM(a*a)) should be (SomeM(25))
    }
    it("should be none if elem is some") {
      (NoneM:OptionM[Int]) flatMap (a => SomeM(a*a)) should be (NoneM)
    }
  }
  describe("flatMap of elem A with function f:A=>SomeM[B]") {
    it("should be some of f(elem) if elem is some") {
      (SomeM(5):OptionM[Int]) flatMap (a => SomeM(a.toString)) should be (SomeM("5"))
    }
    it("should be none if elem is some") {
      (NoneM:OptionM[Int]) flatMap (a => SomeM(a.toString)) should be (NoneM)
    }
  }
  describe("getOrElse of elem of type A with default of type B") {
    it("should give elem present") {
      SomeM(5) getOrElse("5") should be (5)
    }
    it("should give default when not present") {
      (NoneM:OptionM[Int]) getOrElse("5") should be ("5")
    }
  }
  describe("orElse of elem of type A with default of type SomeM[B]") {
    it("should give SomeM[elem] when present") {
      SomeM(5) orElse(SomeM("5")) should be (SomeM(5))
    }
    it("should give SomeM[default] when not present") {
      (NoneM:OptionM[Int]) orElse(SomeM("5")) should be (SomeM("5"))
    }
  }
  describe("filter of elem of type A") {
    it("should give SomeM[elem] if f(elem) is true") {
      SomeM(5) filter(_ == 5) should be (SomeM(5))
    }
    it("should give NoneM if f(elem) is false") {
      SomeM(5) filter(_ == 6) should be (NoneM)
    }
    it("should give NoneM if elem is NoneM") {
      NoneM filter(_ == 5) should be (NoneM)
    }
  }

}


