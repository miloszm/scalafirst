package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz.Scalaz._
import scalaz._

/**
 * Created by mm.
 *
 * http://eed3si9n.com/learning-scalaz/The+Essence+of+the+Iterator+Pattern.html
 */
class IteratorPatternTest {

  /**
   * Scalaz implements Monoid[m].applicative to turn any monoids into an applicative
   */
  @Test
  def testIteratorPattern():Unit = {

    assertTrue(List(1,1) == Monoid[List[Int]].applicative.ap2(List(1), List(1))(Nil))

    assertTrue((List(1),Some(1)) == Applicative[List].product[Option].point(1)) // sth like vector x operator from linear algebra

    assertTrue((List(1, 1),Some(2)) == ((List(1), 1.some) |@| (List(1), 1.some)) {_ |+| _})

    val r = ((List(1), 1.success[String]) |@| (List(1), "boom".failure[Int])) {_ |+| _}
    assertTrue((List(1,1), Failure("boom")) == r)

    val r2 = Applicative[List].compose[Option].point(1)
    assertEquals(List(Some(1)), r2)

  }

  /**
   * traverse
   */
  @Test
  def testTraverse(): Unit = {

    val t1 = List(1, 2, 3) traverse { x => (x > 0) option (x + 1) }

    assertEquals(Some(List(2,3,4)), t1)

    val t2 = List(1, 2, 0) traverse { x => (x > 0) option (x + 1) }

    assertEquals(None, t2)

  }

  /**
   * contents
   */
  @Test
  def testContents(): Unit = {

    def contents[F[_]: Traverse, A](f: F[A]): List[A] = Monoid[List[A]].applicative.traverse(f) {List(_)}
    def contents2[F[_]: Traverse, A](f: F[A]): List[A] = f.traverse[({type l[X]=List[A]})#l, A] {List(_)}

    val tree: Tree[Char] = 'P'.node('O'.leaf, 'L'.leaf)

    println(contents(tree))
    println(contents2(tree))

  }

  /**
   * pound notation case
   */
  @Test
  def testPoundNotation(): Unit = {
    class A {
      class B

      def f(b: B) = "Got my B!"
      def g(b: A#B) = "Got a B."
    }

    val a1 = new A
    val a2 = new A

    /**
     * does not compile because a1.B is not a subclass of instance a2 (this)
     */
    //a2.f(new a1.B)

    /**
     * works fine thanks to the # notation - g accepts B of any instance, not just a2 (this)
     */
    assertEquals("Got a B.", a2.g(new a1.B))
  }

  /**
   * shape
   */
  @Test
  def testShape(): Unit ={

    def shape[F[_]: Traverse, A](f: F[A]): F[Unit] = f traverse {_ => ((): Id[Unit])}

    val tree: Tree[Char] = 'P'.node('O'.leaf, 'L'.leaf)

    println(shape(tree).drawTree)

  }


}
