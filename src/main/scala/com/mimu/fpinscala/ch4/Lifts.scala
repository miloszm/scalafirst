package com.mimu.fpinscala.ch4

object Lifts extends App {

  /**
    * 4.3
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap (aa => b map ( bb => f(aa, bb)))
  }

  /**
    * 4.3
    */
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    }
    yield {
      f(aa,bb)
    }
  }

  /**
    * 4.4
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x::t => map2(x,sequence(t))((a,b) => a :: b)
  }
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((a,b) => map2(a,b)(_ :: _))
  }

  println(sequence(List(Some(1), Some(2), Some(3))))
  println(sequence(List(Some(1), Some(2), Some(3), None, Some(5))))
  println(sequence_2(List(Some(1), Some(2), Some(3))))
  println(sequence_2(List(Some(1), Some(2), Some(3), None, Some(5))))

  /**
    * 4.5
    */
  def traverse[A,B](a:List[A])(f: A => Option[B]):Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x::(t:List[A]) => map2(f(x),traverse(t)(f))(_ :: _)
  }
  def traverse_2[A,B](a:List[A])(f: A => Option[B]):Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  }

  println(traverse(List(1,2,3,4,5))( x => if ((x % 2) != 0) Some(x) else None))
  println(traverse(List(1,2,3,4,5))( Some(_)))
  println(traverse_2(List(1,2,3,4,5))( x => if ((x % 2) != 0) Some(x) else None))
  println(traverse_2(List(1,2,3,4,5))( Some(_)))

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }

  println(sequenceUsingTraverse(List(Some(1), Some(2), Some(3))))
  println(sequenceUsingTraverse(List(Some(1), Some(2), Some(3), None, Some(5))))

}
