package com.mimu.collections

import org.junit.Assert._
import org.junit.Test

trait Animal
trait FurryAnimal extends Animal
case class Dog(name:String) extends Animal
case class Cat(name:String) extends Animal


/**
 * Created by mm.
 */
class CollectionTest {

  /**
   * 10.5 SC Declaring a type when creating a collection
   */
  @Test
  def testCollections_SC_10_5(): Unit = {

    val x = List(1,3.1,33D,500L)
    println(x)


    val a = Array(Dog("Azor"), Cat("Plush"))
    println(a.getClass.getCanonicalName) // type of A is Product
    println(a.getClass.getComponentType)

    val b = Array[Animal](Dog("Azor"), Cat("Plush"))
    println(b.getClass) // type of A is Animal
    println(b.getClass.getComponentType) // type of A is Animal

  }

  @Test
  def testCollections_SC_10_6(): Unit = {
    var sisters = Vector("Melinda")
    sisters = sisters :+ "Melissa"
    sisters = sisters :+ "Marissa"
    // sisters(0) = "Molly" // won't compile
    println(sisters)
    val sisters2 = Vector("Melinda")
    //sisters2 = sisters2 :+ "Melissa" // won't compile
  }

  /**
   * when in doubt and immutable, use vector
   */
  @Test
  def testCollections_SC_10_7(): Unit = {
    val v = IndexedSeq(1,2,3)
    val l = Seq(1,2,3)
    assertTrue(v.isInstanceOf[Vector[Int]])
    assertTrue(l.isInstanceOf[List[Int]])

    val v2 = v.updated(0,5)
    assertEquals(Vector(5,2,3),v2)
    assertEquals(IndexedSeq(5,2,3),v2)
  }

  /**
   * when in doubt and mutable, use ArrayBuffer
   *
   * just as Vector is the recommended "go to" class for immutable, sequential collections,
   * the ArrayBuffer class is recommended as the general-purpose class for mutable sequential collections
   * - from Scala Cookbook - Alvin Alexander
   */
  @Test
  def testCollections_SC_10_8(): Unit = {
    import scala.collection.mutable.ArrayBuffer
    val nums = ArrayBuffer(1,2,3)
    nums += 4
    assertEquals(Vector(1,2,3,4),nums)

    nums += (5,6)
    assertEquals(Vector(1,2,3,4,5,6),nums)

    nums ++= List(7,8)
    assertEquals(Vector(1,2,3,4,5,6,7,8),nums)

    nums -= 9
    assertEquals(Vector(1,2,3,4,5,6,7,8),nums) // was not there so no difference

    nums -= 8
    assertEquals(Vector(1,2,3,4,5,6,7),nums) // this time it was there

    nums --= Array(5,6)
    assertEquals(Vector(1,2,3,4,7),nums)
  }

  /**
   * foreach method applies function to each element of the collection
   * it does not return a value, it is used for its side effect
   *
   * defined in trait IterableLike
   * def foreach[U](f : scala.Function1[A, U]) : scala.Unit
   *
   */
  @Test
  def testCollections_SC_10_9(): Unit = {

    val v = Vector(1,2,3)
    val u:Unit = v.foreach(println)
    assertEquals("()",u.toString)

    val m = Map("a" -> "aa", "b" -> "bb")

    /**
     * interesting that no match is needed on this case
     * this pattern patching is really cool here
     */
    m.foreach {
      case (lefty, righty) => println(s"$lefty -----> $righty")
    }

    m.foreach {
      case x => println(s"${x._1} =====> ${x._2}")
    }
  }




}
