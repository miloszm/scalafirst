package com.mimu.scalaz

import org.junit.Assert._
import org.junit.Test

import scalaz._
import Scalaz._

/**
 * Created by mm.
 *
 *
 */
class ReaderFunctorTest {

  /**
   * (->) r functor
   */
  @Test
  def testReaderFunctor():Unit = {

    val f = (_: Int) * 5
    val g = (_: Int) + 3
    assertEquals(5*(8+3), (g map f)(8))

    val ff = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}
    assertEquals(3*2 + (3+10), ff(3))

    /**
     * monadic behavior of partially applied functions
     *
     * function is considered a value with a context
     *
     * the function monad is also called the reader monad. All the functions read from a common source
     * this is really cool
     */
    val addStuff: Int => Int = for {
      a <- (_: Int) * 2
      b <- (_: Int) + 10
    } yield a + b
    assertEquals(19, addStuff(3))

  }

  /**
   * reader monad - as in LYAHFGG Chapter 14 Reader? Ugh, Not This Joke Again
   */
  @Test
  def testReaderMonad():Unit = {

    val addStuff: Int => Int = for {
      a <- (_: Int) * 2
      b <- (_: Int) + 10
    } yield a + b

    assertEquals(19, addStuff(3))

  }

  /**
   * reader from day 10 of http://eed3si9n.com/learning-scalaz/Monad+transformers.html
   */
  @Test
  def testReaderMonad2():Unit = {

    /**
     * type Reader[E, A] = scalaz.ReaderT[scalaz.Id.Id, E, A]
     *
     * type ReaderT[F[_], E, A] = scalaz.Kleisli[F, E, A]
     *
     * final case class Kleisli[M[_], A, B](val run : scala.Function1[A, M[B]]) extends scala.AnyRef with scala.Product with scala.Serializable {
  def dimap[C, D](f : scala.Function1[C, A], g : scala.Function1[B, D])(implicit b : scalaz.Functor[M]) : scalaz.Kleisli[M, C, D] = { /* compiled code */ }
  def >=>[C](k : scalaz.Kleisli[M, B, C])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def andThen[C](k : scalaz.Kleisli[M, B, C])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def >==>[C](k : scala.Function1[B, M[C]])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def andThenK[C](k : scala.Function1[B, M[C]])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def <=<[C](k : scalaz.Kleisli[M, C, A])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, C, B] = { /* compiled code */ }
  def compose[C](k : scalaz.Kleisli[M, C, A])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, C, B] = { /* compiled code */ }
  def <==<[C](k : scala.Function1[C, M[A]])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, C, B] = { /* compiled code */ }
  def composeK[C](k : scala.Function1[C, M[A]])(implicit b : scalaz.Bind[M]) : scalaz.Kleisli[M, C, B] = { /* compiled code */ }
  def traverse[F[_]](f : F[A])(implicit M : scalaz.Applicative[M], F : scalaz.Traverse[F]) : M[F[B]] = { /* compiled code */ }
  def =<<(a : M[A])(implicit m : scalaz.Bind[M]) : M[B] = { /* compiled code */ }
  def map[C](f : scala.Function1[B, C])(implicit M : scalaz.Functor[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def mapK[N[_], C](f : scala.Function1[M[B], N[C]]) : scalaz.Kleisli[N, A, C] = { /* compiled code */ }
  def flatMapK[C](f : scala.Function1[B, M[C]])(implicit M : scalaz.Bind[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def flatMap[C](f : scala.Function1[B, scalaz.Kleisli[M, A, C]])(implicit M : scalaz.Bind[M]) : scalaz.Kleisli[M, A, C] = { /* compiled code */ }
  def lift[L[_]](implicit evidence$1 : scalaz.Applicative[L]) : scalaz.Kleisli[({ type λ[α] = L[M[α]] })#λ, A, B] = { /* compiled code */ }
  def unlift[N[_], FF[_]](implicit M : scalaz.Comonad[N], ev : scalaz.Liskov.<~<[Kleisli.this.type, scalaz.Kleisli[({ type λ[α] = N[FF[α]] })#λ, A, B]]) : scalaz.Kleisli[FF, A, B] = { /* compiled code */ }
  def unliftId[N[_]](implicit M : scalaz.Comonad[N], ev : scalaz.Liskov.<~<[Kleisli.this.type, scalaz.Kleisli[({ type λ[α] = N[α] })#λ, A, B]]) : scalaz.Reader[A, B] = { /* compiled code */ }
  def rwst[W, S](implicit M : scalaz.Functor[M], W : scalaz.Monoid[W]) : scalaz.ReaderWriterStateT[M, A, W, S, B] = { /* compiled code */ }
  def state(implicit M : scalaz.Functor[M]) : scalaz.StateT[M, A, B] = { /* compiled code */ }
  def liftMK[T[_[_], _]](implicit T : scalaz.MonadTrans[T], M : scalaz.Monad[M]) : scalaz.Kleisli[({ type λ[a] = T[M, a] })#λ, A, B] = { /* compiled code */ }
  def local[AA](f : scala.Function1[AA, A]) : scalaz.Kleisli[M, AA, B] = { /* compiled code */ }
  def endo(implicit M : scalaz.Functor[M], ev : scalaz.Liskov.>~>[A, B]) : scalaz.Endomorphic[({ type λ[α, β] = scalaz.Kleisli[M, α, β] })#λ, A] = { /* compiled code */ }
  def liftF(implicit F : scalaz.Functor[({ type λ[a] = scalaz.Kleisli[M, A, a] })#λ]) : scalaz.Free[({ type λ[a] = scalaz.Kleisli[M, A, a] })#λ, B] = { /* compiled code */ }
     */

    def myName(step: String): Reader[String, String] = Reader {step + " aa " + _}

    def localExample: Reader[String, (String, String, String)] = for {
      a <- myName("First")
      b <- myName("Second") >=> Reader { _ + "dy"}
      c <- myName("Third")
    } yield (a, b, c)

    assertEquals("First aa Fred,Second aa Freddy,Third aa Fred",localExample("Fred"))
  }

  /**
   * Let's do the same example with my own Reader monad named MmReader
   */
  @Test
  def testReaderMonad3():Unit = {

    type MmReader[A, B] = ReaderT[scala.collection.immutable.List, A, B]
    object MmReader extends KleisliInstances with KleisliFunctions {
      def apply[A, B](f: A => scala.collection.immutable.List[B]): MmReader[A, B] = kleisli(f)
    }

    def myName(step: String): MmReader[String, String] = MmReader {(a:String) => List(step + "aa" + a)}

    def localExample: MmReader[String, (String, String, String)] = for {
      a <- myName("First")
      b <- myName("Second")
      c <- myName("Third")
    } yield (a, b, c)

    println(localExample)
    assertEquals(List(("FirstaaFred","SecondaaFred","ThirdaaFred")),localExample("Fred"))

  }


}
