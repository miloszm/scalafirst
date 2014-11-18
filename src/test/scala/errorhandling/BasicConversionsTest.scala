package errorhandling

import org.junit.Test
import org.junit.Assert._

import scala.util.Try

/**
 * Created by mm.
 *
 * Chapter 4 of FPiS
 */
class BasicConversionsTest {

  /**
   * convert String to Option[Int]
   *
   * "234" ---> 234
   * "abc" ---> None
   * ""    ---> None
   */
  @Test
  def convertStringToIntOption(): Unit ={

    def str2IntOpt(str: String):Option[Int] = {
      val v:Option[Int] = Try(str.toInt).toOption
      v
    }

    println(str2IntOpt("234"))
    println(str2IntOpt(""))
    println(str2IntOpt("abc"))

    assertEquals(Some(234),str2IntOpt("234"))
    assertEquals(None     ,str2IntOpt(""))
    assertEquals(None     ,str2IntOpt("abc"))

  }

  /**
   * convert Option[String] to Option[Int]
   *
   * Some("234") ---> 234
   * Some("abc") ---> None
   * None        ---> None
   */
  @Test
  def convertStringOptionToIntOption(): Unit ={

    def str2IntOpt(str: String):Option[Int] = Try(str.toInt).toOption

    def strOpt2IntOpt(strOpt:Option[String]):Option[Int] = {
      str2IntOpt(strOpt.getOrElse(null))
    }

    println(strOpt2IntOpt(Some("234")))
    println(strOpt2IntOpt(None))
    println(strOpt2IntOpt(Some("abc")))

    assertEquals(Some(234),strOpt2IntOpt(Some("234")))
    assertEquals(None     ,strOpt2IntOpt(None))
    assertEquals(None     ,strOpt2IntOpt(Some("abc")))

  }

  /**
   * convert Option[String] to Option[Int] (2)
   *
   * Some("234") ---> 234
   * Some("abc") ---> None
   * None        ---> None
   */
  @Test
  def convertStringOptionToIntOption2(): Unit ={

    def str2Int(str: String):Option[Int] = {
      Try(str.toInt).toOption
    }

    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

    def extract2[A](a:Option[Option[A]]): Option[A] = {
      a match {
        case s: Some[Option[A]] => s.get
        case None => None
      }
    }

    def extract[A](a:Option[Option[A]]): Option[A] = { a.getOrElse(None) }

    def extract4[A](a:Option[Option[A]]): Option[A] = { a flatMap((x)=>x) }

    println(extract(lift(str2Int)(Some("234"))))
    println(extract(lift(str2Int)(None)))
    println(extract(lift(str2Int)(Some("abc"))))

    assertEquals(Some(234),extract(lift(str2Int)(Some("234"))))
    assertEquals(None     ,extract(lift(str2Int)(None)))
    assertEquals(None     ,extract(lift(str2Int)(Some("abc"))))


  }

  /**
   * convert Option[String] to Option[Int] (3)
   *
   * Some("234") ---> 234
   * Some("abc") ---> None
   * None        ---> None
   *
   * Final - best version
   */
  @Test
  def convertStringOptionToIntOption3(): Unit ={

    def strOpt2IntOpt(strOpt:Option[String]):Option[Int] = {
      Try(strOpt.getOrElse("").toInt).toOption
    }

    println(strOpt2IntOpt(Some("234")))
    println(strOpt2IntOpt(None))
    println(strOpt2IntOpt(Some("abc")))

    assertEquals(Some(234),strOpt2IntOpt(Some("234")))
    assertEquals(None     ,strOpt2IntOpt(None))
    assertEquals(None     ,strOpt2IntOpt(Some("abc")))


  }

}
