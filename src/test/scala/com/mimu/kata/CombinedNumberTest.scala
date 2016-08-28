package com.mimu.kata

import org.scalatest.{FlatSpec, Matchers}

class CombinedNumberTest extends FlatSpec with Matchers {

  "Combined number" should "return bigest combined number" in {
    CombinedNumber.getBiggest(List(50,2,1,9)) shouldBe "95021"
  }

  "Permutations" should "return all permutations" in {
    CombinedNumber.perms(List(1,2,3)) should contain theSameElementsAs  (List(List(1,2,3),List(1,3,2),List(2,1,3),List(2,3,1),List(3,2,1),List(3,1,2)))
  }

//  "insertIInAllPositions" should "return lists with i in all positions" in {
//    CombinedNumber.insertIInAllPos(3, List(1,2), List(List())) should contain theSameElementsAs
//  }
}
