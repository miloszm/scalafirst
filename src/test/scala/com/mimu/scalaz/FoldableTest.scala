package com.mimu.scalaz

import org.junit.Test
import scalaz._
import Scalaz._


/**
 * Created by mm.
 *
 * Foldable
 */
class FoldableTest {

  /**
   * foldable
   */
  @Test
  def testFoldable():Unit = {

    /**
     * test fold on content as monoidal boolean with operation ||
     */
    val r:Boolean @@ Tags.Disjunction = List(true, false, true, true) foldMap {Tags.Disjunction.apply}

    Tag.unwrap(r) assert_=== true

    /**
     * test fold on content as monoidal boolean with operation &&
     */
    val r2:Boolean @@ Tags.Conjunction = List(true, false, true, true) foldMap {Tags.Conjunction.apply}

    Tag.unwrap(r2) assert_=== false

  }

}
