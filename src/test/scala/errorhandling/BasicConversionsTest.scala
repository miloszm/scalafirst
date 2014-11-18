package errorhandling

import org.junit.Test

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

    def str2IntOpt(str: String):Option[Int] = {
      val v:Option[Int] = Try(str.toInt).toOption
      v
    }

    def strOpt2IntOpt(strOpt:Option[String]):Option[Int] = {
      str2IntOpt(strOpt.getOrElse(null))
    }

    println(strOpt2IntOpt(Some("234")))
    println(strOpt2IntOpt(None))
    println(strOpt2IntOpt(Some("abc")))

  }

}
