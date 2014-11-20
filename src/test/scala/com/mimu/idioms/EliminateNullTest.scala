package com.mimu.idioms

import org.junit.Test
import org.junit.Assert._

import scala.util.{Success, Try, Failure}


/**
 * Created by mm.
 */
class EliminateNullTest {

  /**
   * 20.5 SC - Eliminate null values from your code
   */
  @Test
  def testEliminateNull(): Unit = {
    val str: Option[String] = None // you need to specify a type in such case
    println(str)


    def readTextFile(filename: String ): Try[List[String]] = {
      Try(io.Source.fromFile(filename).getLines.toList)
    }

    readTextFile("bogus file name") match {
      case Success(lines) => lines.foreach(println)
      case Failure(f) => println(f)
    }

  }

  /**
   * 20.6 SC - using the Option/Some/None pattern
   *
   * Option/Some/None
   * Try/Success/Failure
   */
  @Test
  def test_20_6(): Unit = {

    def toInt(s:String): Option[Int] = {
      try {
        Some(Integer.parseInt(s.trim))
      }
      catch {
        case e: Exception => None
      }
    }

    def toInt2(s:String): Option[Int] = {
      Try(Integer.parseInt(s.trim)).toOption
    }

    assertEquals(None, toInt("boo"))
    assertEquals(None, toInt(null))
    assertEquals(Some(3), toInt("3"))

    assertEquals(None, toInt2("boo"))
    assertEquals(None, toInt2(null))
    assertEquals(Some(3), toInt2("3"))

    println(toInt2("boo").getOrElse(4))

    toInt2("3").foreach{ println }
    toInt2("").foreach{ println }

    toInt2("boo") match {
      case Some(i) => println(i)
      case None => println("conversion to int failed")
    }

  }

  /**
   * 20.6 SC - using the Option/Some/None pattern
   *
   */
  @Test
  def test_20_6_collections(): Unit = {

    def toInt(s:String): Option[Int] = {
      Try(Integer.parseInt(s.trim)).toOption
    }

    /**
     * this is a really good example to show the usefulness of flatMap
     */
    val bag = List("1", "boo", "3")
    println(s"map ........... ${bag.map(toInt)}")
    println(s"map flatten ... ${bag.map(toInt).flatten}")
    println(s"flatMap ....... ${bag.flatMap(toInt)}")
    println(s"map collect ... ${bag.map(toInt).collect{ case Some(i) => i }}")

  }

  /**
   * 20.6 SC - using the Option/Some/None pattern
   *
   */
  @Test
  def test_20_6_Try_Success_Failure(): Unit = {

    println(s"Try(1000/0) ........... ${for ( i <- Try(1000/0)) yield i}")
    println(s"Try(1000/2) ........... ${for ( i <- Try(1000/2)) yield i}")

    Try(1000/0).foreach((x) => println("dividing by zero"))
    Try(1000/3).foreach(println)

    Try(1000/0) match {
      case Failure(f) => println(f)
      case Success(x) => println(x)
    }

    Try(1000/4) match {
      case Failure(f) => println(f)
      case Success(x) => println(x)
    }

  }

  /**
   * 20.6 SC - using the Option/Some/None pattern (2)
   *
   */
  @Test
  def test_20_6_Try_Success_Failure_2(): Unit = {

    /**
     * what is interesting here is that although i is Int, yielded collection is Try (!!)
     */
    println(s"Try(1000/2) ........... ${for ( i <- Try(1000/2)) yield i}")

    val sx = "100"
    val sy = "200"
    val y = for { a <- Try(sx.toInt); b <- Try(sy.toInt)} yield a * b
    println(y.getOrElse(0))

    val sx2 = "100"
    val sy2 = "boo"
    val y2:Try[Int] = for { a <- Try(sx2.toInt); b <- Try(sy2.toInt) } yield a * b
    /**
     * note - yielded is Try[Int], not some List[Int] - this is really cool !!
     */
    y2 match {
      case Failure(f) => println(f)
      case Success(x) => println(x)
    }
  }
}
