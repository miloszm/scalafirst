package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz.{Functor, ProductFunctor}

/**
 * I do not import scalaz on purpose
 */
//import scalaz.Scalaz._
//import scalaz._

trait ProductFunctor[F[_], G[_]] extends Functor[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) = (F.map(fa._1)(f), G.map(fa._2)(f))
}


trait Functor[F[_]]{ self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /**The product of Functors `F` and `G`, `[x](F[x], G[x]])`, is a Functor */
  def product[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = (F[α], G[α])})#λ] = new ProductFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }
}

class FunctorListImpl extends Functor[List] {
  override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
}

object FunctorListImpl {
  def apply():FunctorListImpl = new FunctorListImpl
}

class FunctorOptionImpl extends Functor[Option] {
  override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
}

object FunctorOptionImpl {
  def apply():FunctorOptionImpl = new FunctorOptionImpl
}

/**
 * Created by mm.
 *
 */
class FunctorExperimentTest {



  /**
   * functor
   */
  @Test
  def testFunctor():Unit = {

    println(FunctorListImpl().map(List(1,2,3))(_ * 2))

  }


  /**
   * product of functors
   */
  @Test
  def testProductOfFunctors():Unit = {

    val p = FunctorListImpl().product(FunctorOptionImpl())

    println(p.map((List(1,2,3),Option(5)))((x:Int) => x*3))

  }

}
