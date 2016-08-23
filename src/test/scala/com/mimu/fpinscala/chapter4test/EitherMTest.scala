package com.mimu.fpinscala.chapter4test

import com.mimu.fpinscala.{LeftM, RightM}
import org.scalatest.{FunSpec, Matchers}

/**
  * 4.6
  */

class EitherMTest extends FunSpec with Matchers {

  describe("map of elem A with function f:A=>A") {
    it("should be some of f(elem) if elem is some") {
      RightM[Int](5) map (a => a*a) should be (RightM(25))
    }
    it("should be none if elem is some") {
      (LeftM[String]("")) map ((a:Int) => a*a) should be (LeftM(""))
    }
  }
  describe("map of elem A with function f:A=>B") {
    it("should be some of f(elem) if elem is some") {
      RightM[Int](5) map (_.toString) should be (RightM("5"))
    }
    it("should be none if elem is some") {
      (LeftM[String]("")) map (_.toString) should be (LeftM(""))
    }
  }
  describe("flatMap of elem A with function f:A=>SomeM[A]") {
    it("should be some of f(elem) if elem is some") {
      RightM(5) flatMap (a => RightM(a*a)) should be (RightM(25))
    }
    it("should be none if elem is some") {
      (LeftM[String]("")) flatMap ((a:Int) => RightM(a*a)) should be (LeftM(""))
    }
  }
  describe("flatMap of elem A with function f:A=>SomeM[B]") {
    it("should be some of f(elem) if elem is some") {
      (RightM(5):RightM[Int]) flatMap (a => RightM(a.toString)) should be (RightM("5"))
    }
    it("should be none if elem is some") {
      (LeftM[String]("")) flatMap (a => RightM(a.toString)) should be (LeftM(""))
    }
  }
  describe("orElse of elem of type A with default of type SomeM[B]") {
    it("should give SomeM[elem] when present") {
      RightM(5) orElse(RightM("5")) should be (RightM(5))
    }
    it("should give SomeM[default] when not present") {
      (LeftM[String]("")) orElse(RightM("5")) should be (RightM("5"))
    }
  }

}


