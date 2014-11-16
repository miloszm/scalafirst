package com.mimu.types

import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm.
 */
class SelfTypeTest {

  /**
   * 18.10 SftI - Self Types
   *
   * Syntax "this:A" may be used to make sure that trait
   * will be mixed into classes which are T or descendants of T
   */
  @Test
  def testBasicSelfType():Unit = {

    class Tracee {

    }

    class NonTrecee {

    }

    trait Traced {
      this: Tracee => def showTrace():String = "trace"
    }

    class SubTracee extends Tracee with Traced {

    }

    val t1 = new SubTracee()
    assertEquals("trace", t1.showTrace())

    /**
     * the following won't compile as Traced can be only mixed in in classes where this <: Tracee
     */
//    class SubNonTracee extends NonTrecee with Traced {
//
//    }

  }

  /**
   * 18.10 SftI - Self Types
   *
   * Syntax "this:A with B with ... can be used to make sure
   * that trait will be mixed into classes which are "A with B with ..."
   */
  @Test
  def testBasicSelfType2():Unit = {

    class Tracee {

    }

    trait Tracee2 {

    }

    trait Traced {
      this: Tracee with Tracee2 => def showTrace():String = "trace"
    }

    class SubTracee extends Tracee with Tracee2 with Traced {

    }

    val t1 = new SubTracee()
    assertEquals("trace", t1.showTrace())

    /**
     * the following won't compile as Traced can be only mixed in in classes where this <: Tracee
     * and trait Tracee2 is mixed in
     */
//    class SubTracee2 extends Tracee with Traced {
//
//    }

  }

  /**
   * 18.10 SftI - Self Types
   *
   * Rather than using "this" at point // LAB01 we use "someid"
   * it works the same as "this", but in addition can be used by
   * member classes as pointer to this
   */
  @Test
  def testBasicSelfType3():Unit = {

    class SomeClass {
      def showSth():String = "sth"
    }

    class SomeClassX {
      def showSth():String = "sthx"
    }

    trait SomeTrait {
      someid: SomeClass =>   // LAB01
        class SomeMemberClass {
          def giveMeIncludingThis() = someid
        }
      val smc = new SomeMemberClass
    }

    class SomeClass2 extends SomeClass with SomeTrait {

    }

    /**
     * won't compile as SomeTrait requires to be mixed in SomeClass or descendants
     */
//    class SomeClass3 extends SomeClassX with SomeTrait {
//
//    }

    val sc2 = new SomeClass2
    assertEquals("sth", sc2.smc.giveMeIncludingThis().showSth())

  }

}
