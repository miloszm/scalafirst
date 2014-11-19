package com.mimu.classes

import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm.
 */
class CaseClassesTest {

  /**
   * 4.14 SC Case classes
   */
  @Test
  def basicCaseClassTest(): Unit = {

    case class Person(name: String, relation: String)

    val p = Person("Lorena", "Friend")

    println(p)

    val r = p.copy(relation = "Dear Friend")

    println(r)

    r match { case Person(n,r) => println(s"unapply: $n $r") }

    println(p == r)

    val s = r.copy(relation = "Friend")

    println(p == s)

    println(Person.tupled)

  }

  /**
   * 4.15 SC Defining an equals method
   */
  @Test
  def basicEqualsMethodTest(): Unit = {

    class Person(name: String, age: Int){

      def canEqual(a:Any) = a.isInstanceOf[Person]

      override def equals(that:Any): Boolean =
         that match {
           case that: Person => that.canEqual(this) && this.hashCode() == that.hashCode()
           case _ => false
         }

      override def hashCode:Int = {
        val prime = 31
        prime * (prime + age) + (if (name == null) 0 else name.hashCode)
      }

    }

    val p1 = new Person("Lorena", 37)
    val p2 = new Person("Lorena", 37)

    assertTrue(p1 == p2)

  }

  /**
   * 4.16 SC Inner classes
   */
  @Test
  def basicInnerClassesTest(): Unit = {

    class Container {
      class Containee {
        var x = 1
      }
    }

    val oc1 = new Container
    val oc2 = new Container
    val ic1 = new oc1.Containee
    val ic2 = new oc2.Containee
    println(ic1)
    println(ic2)
    ic1.x = 14
    println(s"ic1.x = ${ic1.x}")

  }

}
