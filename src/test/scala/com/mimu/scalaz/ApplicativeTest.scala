package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._

/**
 * Created by mm.
 *
 * Applicative Functor
 *
 *
 *
 * NOTE - we are not using scalaz implicits here but rather we
 * provide our own implementations of Functor[Option] and Application[Option]
 *
 * Applicative
 * ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B]
 *
 * compare with functor:
 * map[A, B](fa: F[A])(f: A => B): F[B]
 *
 */
class ApplicativeTest {

  /**
   * let's test option functor with function
   *
   * map[A, B](fa: F[A])(f: A => B): F[B]
   *
   * our f is a function which takes two arguments
   * our B is a partially applied function
   * can we do that in Scala ?
   */
  @Test
  def testFunctorWithPartiallyAppliedFunction():Unit = {

    val functor = Functor[Option](createFunctorForOption)

    def fun(x:String)(y:String):String = {
      x + y
    }

    /**
     * input to our mapping is a 2 arg function which will be partially applied
     */
    val result = functor.map(Some("abc"))(fun)
    println(s"result of mapping is: $result")

    result match {
      case Some(x) => println(s"result of mapping inside Some is: $x")
      case _ => fail
    }

    /**
     * now we can extract the content of some - it will be a partially
     * applied function, and apply the remaining parameter to it
     * it should produce the desired result of concatenating "abc" and "def"
     */
    println(result.getOrElse({s:String => "zzz"}).apply("def"))

  }

  /**
   * curried test
   */
  @Test
  def testCurried(): Unit ={

    val listOfPartiallyAppliedFunctions = List(1, 2, 3, 4) map {(_: Int) * (_:Int)}.curried

    println(listOfPartiallyAppliedFunctions)

    val resultList = listOfPartiallyAppliedFunctions map {_(3)}

    println(resultList)

    assertEquals(List(3,6,9,12), resultList)

  }


  /**
   * curried test 2
   */
  @Test
  def testCurried2(): Unit ={

    def fun(x:Int)(y:Int):Int = x * y

    val listOfPartiallyAppliedFunctions = List(1, 2, 3, 4) map { fun _ }

    println(listOfPartiallyAppliedFunctions)

    val resultList = listOfPartiallyAppliedFunctions map {_(4)}

    println(resultList)

    assertEquals(List(4,8,12,16), resultList)

  }


  /**
   * summary:
   *
   */
  @Test
  def testApplicativeFunctor():Unit = {

    val appFunctor = Applicative[Option](createApplicativeFunctorForOption)

    def fun(x:String):String = {
      "Scala " + x
    }

    val result = appFunctor.ap(Some("Rules"))(Some(fun _)) // underscore needed to treat fun as a partially applied function
    println(result)
    assertEquals(Some("Scala Rules"),result)

  }

  /**
   * summary:
   *
   */
  @Test
  def testApplicativeFunctorChangingType():Unit = {

    val appFunctor = Applicative[Option](createApplicativeFunctorForOption)

    def fun(x:String):Int = {
      x.length
    }

    val result = appFunctor.ap(Some("ScalaRules"))(Some(fun _)) // underscore needed to treat fun as a partially applied function
    println(result)
    assertEquals(Some(10),result)

  }

  /**
   * creates applicative functor for option
   * in order to avoid using implicits from scalaz
   * when creating Applicative[Option], second argument is implicit
   * and the result of this method may be used instead
   * this gives us self-containment of this test (for pedagogical reasons)
   */
  def createApplicativeFunctorForOption: Applicative[Option] with Object {def point[A](a: => A): Option[A]; def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B]} = {

    new Applicative[Option] {
      override def point[A](a: => A): Option[A] = {
        Some(a)
      }

      override def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B] = {
        for (
          a <- fa;
          ff <- f
        )
        yield ff(a)
      }
    }
  }

  /**
   * creates functor for option
   * in order to avoid using implicits from scalaz
   * when creating Functor[Option], second argument is implicit
   * and the result of this method may be used instead
   * this gives us self-containment of this test (for pedagogical reasons)
   */
  def createFunctorForOption: Functor[Option] with Object {def map[A, B](fa: Option[A])(f: (A) => B): Option[B]} = {
    new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
        for (e <- fa) yield f(e)
      }
    }
  }

  /**
   * misc applicative
   *
   * from: http://eed3si9n.com/learning-scalaz/Applicative.html
   */
  @Test
  def miscApplicativeTest(): Unit ={
    import Scalaz._
    assertEquals(List(1)  ,         1.point[List])
    assertEquals(1.some   ,         1.point[Option])
    assertEquals(List(5)  ,         1.point[List] map {_ + 4})
    assertEquals(5.some   ,         1.point[Option] map {_ + 4})

    assertEquals(12.some  ,         9.some <*> {(_: Int) + 3}.some)
    println({(_: Int) + 3}.some)

    assertEquals(1.some   ,         1.some <* 2.some)
    assertEquals(2.some   ,         1.some *> 2.some)

    assertEquals(21.some  ,         12.some <*> { 9.some <*> {(_: Int) + (_: Int)}.curried.some })
    assertEquals(108.some ,         12.some <*> { 9.some <*> {(_: Int) * (_: Int)}.curried.some })
    assertEquals(102.some ,         (3.some |@| 99.some) {_ + _})

    assertEquals(1.some   ,         1.some)

    assertEquals(List(5,10,15,1,4,9)  ,         List(1, 2, 3) <*> List((_: Int) * 5, (x: Int) => x * x))
    assertEquals(List(3+1,3+2,4+1,4+2,3*1,4*1,3*2,4*2)   ,         List(3, 4) <*> { List(1, 2) <*> List({(_: Int) + (_: Int)}.curried, {(_: Int) * (_: Int)}.curried)})
    assertEquals(List("ha?","ha!","he?","he!")   ,         (List("ha", "he") |@| List("?", "!")) {_ + _})
    assertEquals(1.some   ,         1.some)
    assertEquals(1.some   ,         1.some)

  }

  /**
   * lift2
   */
  @Test
  def testLift2(): Unit ={
    import Scalaz._

    val f = Apply[Option].lift2({(_:Int) + (_:Int)})
    println(f(5.some, 6.some))
    assertEquals(11.some   ,         f(5.some, 6.some))

  }

  /**
   * sequence of applicatives
   *
   * from: http://eed3si9n.com/learning-scalaz/Applicative.html
   *
   * Makes container upside down - inside container is flattened and used as outside container
   *
   * Haskell version:
   * sequenceA :: (Applicative f) => [f a] -> f [a]
   * sequenceA [] = pure []
   * sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
   *
   */
  @Test
  def testSequenceOfApplicatives(): Unit ={
    import Scalaz._

    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
      case Nil     => (Nil: List[A]).point[F]
      case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
    }

    assertEquals(Some(List(1,2))   ,         sequenceA(List(1.some, 2.some)))

    assertEquals(None              ,         sequenceA(List(1.some, 2.some, none)))

    assertEquals(List(List(1),List(2))   ,         sequenceA(List(List(1,2))))

    println(sequenceA(List(List(1,2),List(3,4))))

    assertEquals(List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)), sequenceA(List(List(1,2),List(3,4))))

  }

  /**
   *
   * from eed3si9n.com, some stuff is much easier in Haskell than in Scala
   *
   * list of partially applied functions as containers
   *
   */
  @Test
  def testSequenceOfApplicatives2(): Unit = {
    import Scalaz._

    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
      case Nil     => (Nil: List[A]).point[F]
      case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
    }

    type Function1Int[A] = ({type l[A]=Function1[Int, A]})#l[A] // from eed3si9n.com - why is this so hard in Scala ?

    val s = sequenceA(List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1): List[Function1Int[Int]])

    println(s)

    println(s(3))

    assertEquals(List(6,5,4), s(3))

  }


}
