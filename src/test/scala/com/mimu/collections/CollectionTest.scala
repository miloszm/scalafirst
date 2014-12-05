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

  @Test
  def testCollections_SC_10_10(): Unit = {

    val fruits = Traversable("apple", "banana", "peach")
    val fruitsA = Array("apple", "banana", "peach")
    val fruitsB = IndexedSeq("apple", "banana", "peach")

    /**
     *
     * zipWithIndex:
     *
     * implementation in: trait IndexedSeqOptimized
     *
     * declared in IterableLike as
     *
     *     def zipWithIndex[A1 >: A, That](implicit bf : scala.collection.generic.CanBuildFrom[Repr, scala.Tuple2[A1, scala.Int], That]) : That
     *
     */

    //for ((elem, count) <- fruits.zipWithIndex){ // won't compile as it is not a member of Traversable
    for ((elem, count) <- fruitsA.zipWithIndex){
      println(s"$count $elem (Array) ${fruitsA.getClass()}")
    }
    for ((elem, count) <- fruitsB.zipWithIndex){
      println(s"$count $elem (IndexedSeq) ${fruitsB.getClass()}")
    }

    /**
     * with view
     */
    for ((elem, count) <- fruitsB.view.zipWithIndex){
      println(s"$count $elem (with view)")
    }

    /**
     * zip with Stream from 1
     * (really cool)
     */
    for ((elem, count) <- fruitsB zip (Stream from 1)){
      println(s"$count $elem (index obtained from Stream)")
    }

    val r = for ((elem, count) <- fruitsB zip (Stream from 1)) yield (elem,count)
    println(s"$r + is of class ${r.getClass}")

  }

  @Test
  def testCollections_SC_10_11(): Unit = {

    val l = List("a", "b", "c")
    val zwi = l.zipWithIndex
    println(zwi)

    l.zipWithIndex.foreach {
      d => println(s"${d._2} is ${d._1}")
    }

  }

  /**
   *
   */
  @Test
  def testCollections_SC_10_99(): Unit = {

  }

  /**
   * using iterators
   *
   * type Iterator[+A] = scala.collection.Iterator[A]
   *
   * trait Iterator[+A] extends scala.AnyRef with scala.collection.TraversableOnce[A]
   */
  @Test
  def testCollections_SC_10_12(): Unit = {
    val it = Iterator(1,2,3)
    println(it.getClass) // scala.collection.IndexedSeqLike$Elements

    it.foreach(println)
    it.foreach(println) // this won't print anything as iterator is exhausted

    val ita = Iterator(4,5,6).toArray
    println(ita.getClass()) // class [I
    ita.foreach(println)

    val itb = Iterator(7,8,9).toStream
    println(itb.getClass()) // scala.collection.immutable.Stream$Cons
    itb.foreach(println)

  }

  /**
   * transforming one collection into another with for/yield
   *
   * In general, the collection type that's returned by a for comprehension
   * will be the same type that you begin with
   */
  @Test
  def testCollections_SC_10_13(): Unit = {

    val f = Vector("apple", "banana", "peach")

    val pairs = for (ff <- f) yield (ff, ff.length)
    println(pairs)
    println(pairs.getClass())  // scala.collection.immutable.Vector
    println(pairs.getClass().getComponentType)  // null - does it work only for Arrays ?

    val fa = Array("apple", "banana", "peach")
    val pairsa = for (ff <- fa) yield (ff, ff.length)
    println(pairsa)
    println(pairsa.getClass())  // [Lscala.Tuple2;
    println(pairsa.getClass().getComponentType)  // scala.Tuple2

  }

  /**
   * transforming one collection to another with map
   */
  @Test
  def testCollections_SC_10_14(): Unit = {
    val helpers = Vector("John", "Kate", "Sue")
    val caps = helpers map {e => e.toUpperCase()}
    println(caps.getClass)  // Vector
    println(caps.getClass().getComponentType)  // null
    val lengths = helpers map { e => e.length}
    println(lengths.getClass)  // Vector
    println(lengths.getClass().getComponentType)  // null

    println("HAL".map((c:Char) => (c.toByte+1).toChar))

    val f = Vector("apple", "banana", "peach")
    val newFruits = f.map( f =>
      if (f.length < 6) f.toUpperCase()
    )
    println(newFruits)
    assertTrue(newFruits.contains(())) // contains Unit ()
  }

  /**
   * flattening
   */
  @Test
  def testCollections_SC_10_15(): Unit = {
    val ll = List(List(1,2),List(3,4))
    println(ll.flatten)

    val lll = List("Hello", "World")
    println(lll.flatten)

    val llll = List(Some(4), None, Some(3), None)
    println(llll.flatten)
    assertEquals(List(4,3),llll.flatten)

  }

  /**
   * flatMap
   *
   * note - in flatMap - first map, then flatten !!!
   *
   * the name flatMap is reversed - it is really map then flat - the is the entire caveat
   *
   */
  @Test
  def testCollections_SC_10_16(): Unit = {
    val l = List(Some(4), None, Some(3), None)
    println(l.flatMap((x)=>x).sum)
    println(l.flatten.sum)

    import scala.util.Try

    val bag = List("1", "2", "three", "4", "3")
    def toInt(s:String):Option[Int] = Try(Integer.parseInt(s.trim)).toOption
    println(bag.flatMap(toInt).sum)
    println((bag map(toInt) flatten).sum)
    assertEquals(bag.flatMap(toInt).sum, (bag map(toInt) flatten).sum)

    println(bag.flatMap(toInt).partition(_ > 2))

  }

  /**
   * filter
   */
  @Test
  def testCollections_SC_10_17(): Unit = {

    println(List.range(1,10).filter(_ % 2 == 0))

  }

  /**
   * extracting a sequence of elements from a collection
   */
  @Test
  def testCollections_SC_10_18(): Unit = {

    val x = (1 to 10).toArray

    println(x.getClass)

    x.drop(3).foreach(print)
    println
    x.slice(3,5).foreach(print)
    println(x.headOption)
    println(x.lastOption)

  }

  /**
   * groupBy, partition, etc. - splitting sequences into subsets
   */
  @Test
  def testCollections_SC_10_19(): Unit = {

    val l = List(2,5,9,4,7,1,8)

    assertEquals(Map[Boolean,List[Int]]
      (false -> List(2,5,4,1), true -> List(9,7,8)),
      l.groupBy(_ > 5))

    assertEquals(
      (List(9,7,8), List(2,5,4,1)),
      l.partition(_ > 5))

    assertEquals(
      (List(2,5), List(9,4,7,1,8)),
      l.span(_ <= 5))

    assertEquals(
      (List(2,5), List(9,4,7,1,8)),
      l.splitAt(2))

    val groups = l.groupBy(_ > 5)
    println(groups(true))
    println(groups(false))

    println("sliding step 1:")
    l.sliding(3).foreach(println)
    println("sliding step 2:")
    l.sliding(3,2).foreach(println)

    val ll = List((1,2),('a','b'),(3,4))
    println(ll.unzip)

    val couples = List(("Basia","Mikolaj"),("Beata","Piotr"))
    val (women:List[String],men:List[String]) = couples unzip
    val w = women
    val m = men
    println(w)
    println(m)
    println(women zip men)

  }













}
