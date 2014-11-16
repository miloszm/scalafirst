package com.mimu.types

import org.junit.Assert._
import org.junit.Test

/**
 * Created by mm on.
 */
class TypeTest {

  @Test
  def basicTypeTest(): Unit = {

    case class MyClass(val x:Int)

    val i1:MyClass = MyClass(3)

    type A = MyClass

    val i2:A = MyClass(4)

    /**
     * Section 29.6 PiS p.648
     * The ".type" on the end means that this is a singleton type.
     * A singleton type is very specific and holds only one object, whichever
     * object is refered to by i1.
     */
    val i3:i1.type = i1
    // i3 = i2 // this won't compile as the type of i3 is a singleton type i1.type

    /**
     * The following does not compile:
     */
    //val i4:i2.type = i1

    println(i2)
    println(MyClass(5))
    println(i3)

    assertEquals(i1,i3)

  }

}
