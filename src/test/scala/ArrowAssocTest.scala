import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm on 15/03/2014.
 *
 * Exercise 1 ch 21 SftI
 *
 */
class ArrowAssocTest {

  @Test
  def basicArrowAssocTest(): Unit = {
    val t1 = ("Hi" -> 88)
    println(t1.getClass)

    val t2 = (88 -> "Hi")
    println(t2.getClass)

    case class MyClass(val x:Int)

    val t3 = (MyClass(5) -> 39)
    println(t3.getClass)
    assertTrue(t3.isInstanceOf[Tuple2[MyClass,Int]])
  }


}