package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import scalaz.std.option._

/**
 * Created by mm.
 *
 * Monads provide a solution to the following problem:
 * if we have a value with contaxt, m a, how do we apply it to a function
 * that takes normal a and returns a value with a context
 *
 */
class MonadPoleBirdsTest {

  /**
   * Pole Birds example from the "Learn you a Haskell for great good"
   */


  @Test
  def testPoleBirds():Unit = {

    case class Pole(left:Int, right:Int){
      def >>>(p:Pole):Pole = {
        val pp = Pole(left + p.left, right + p.right)
        //println(s"$this $p $pp")
        pp
      }
    }

    def landLeft (n:Int):Pole = Pole(n, 0 )
    def landRight(n:Int):Pole = Pole(0, n )

    val r1 = Pole(-2,-2) >>> landLeft(3) >>> landRight(5)
    println(r1)
    assertEquals(r1, Pole(1,3))

  }

  @Test
  def testMonadicPoleBirds():Unit = {

    case class Pole(left:Int, right:Int){
      def landLeft (n:Int):Option[Pole] = {
        val s = Some(Pole(left + n, right))
        if (Math.abs((left+n)-right) < 4) s else None
      }
      def landRight(n:Int):Option[Pole] = {
        val s = Some(Pole(left  , right+n ))
        if (Math.abs(left-(right+n)) < 4) s else None
      }
    }

    val r1 = Monad[Option].point(Pole(-2,-2)) flatMap {_.landLeft(3)} flatMap {_.landRight(5)}
    println(r1)
    assertEquals(Some(Pole(1,3)), r1)

    val r2 = Monad[Option].point(Pole(-2,-2)) flatMap {_.landLeft(3)} flatMap {_.landRight(9)}
    println(r2)
    assertEquals(None, r2)

  }



  @Test
  def testMonadicPoleBirds_withBindOp():Unit = {

    /**
     * syntax.ToBindOps implicitly converts F[A] where [F: Bind]
     * into BindOps[F, A] that implements >>= operator.
     */
    import syntax.bind._

    case class Pole(left:Int, right:Int){
      def landLeft (n:Int):Option[Pole] = {
        val s = Some(Pole(left + n, right))
        if (Math.abs((left+n)-right) < 4) s else None
      }
      def landRight(n:Int):Option[Pole] = {
        val s = Some(Pole(left  , right+n ))
        if (Math.abs(left-(right+n)) < 4) s else None
      }
    }

    val r1 = Monad[Option].point(Pole(-2,-2)) >>= {_.landLeft(3)} >>= {_.landRight(5)}
    println(r1)
    assertEquals(Some(Pole(1,3)), r1)

    val r2 = Monad[Option].point(Pole(-2,-2)) >>= {_.landLeft(3)} >>= {_.landRight(9)}
    println(r2)
    assertEquals(None, r2)

  }

}
