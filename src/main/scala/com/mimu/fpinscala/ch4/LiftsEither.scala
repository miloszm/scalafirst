package com.mimu.fpinscala.ch4

import com.mimu.fpinscala.{EitherM, LeftM, RightM}

object LiftsEither extends App {

  /**
    * 4.7
    */
  def traverse[E,A,B](a:List[A])(f: A => EitherM[E,B]):EitherM[E,List[B]] = a match {
    case Nil => RightM(Nil)
    case x::(t:List[A]) => f(x).map2(traverse(t)(f))(_ :: _)
  }
  def traverse_2[E,A,B](a:List[A])(f: A => EitherM[E,B]):EitherM[E,List[B]] = {
    a.foldRight[EitherM[E,List[B]]](RightM(Nil))((h,t) => f(h).map2(t)(_ :: _))
  }

  println(traverse(List(1,2,3,4,5))( x => if ((x % 2) != 0) RightM(x) else LeftM("abc1")))
  println(traverse(List(1,2,3,4,5))( RightM(_)))
  println(traverse_2(List(1,2,3,4,5))( x => if ((x % 2) != 0) RightM(x) else LeftM("abc2")))
  println(traverse_2(List(1,2,3,4,5))( RightM(_)))

  def sequenceUsingTraverse[E,A](a: List[EitherM[E,A]]): EitherM[E,List[A]] = {
    traverse(a)(x => x)
  }

  println(sequenceUsingTraverse(List(RightM(1), RightM(2), RightM(3))))
  println(sequenceUsingTraverse(List(RightM(1), RightM(2), RightM(3), LeftM("abc3"), RightM(5))))

}
