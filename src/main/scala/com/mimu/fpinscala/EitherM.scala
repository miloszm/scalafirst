package com.mimu.fpinscala

/**
  * 4.6
  */
sealed trait EitherM[+E,+A] {

  def map[B](f: A => B):EitherM[E,B] = this match {
    case RightM(v) => RightM(f(v))
    case LeftM(e) => LeftM(e)
  }

  def flatMap[EE >: E, B](f: A => EitherM[EE, B]): EitherM[EE, B] = this match {
    case RightM(v) => f(v)
    case LeftM(e) => LeftM(e)
  }

  def orElse[EE >: E, AA >: A](b: => EitherM[EE, AA]): EitherM[EE, AA] = this match {
    case LeftM(e) => b
    case RightM(v) => RightM(v)
  }

  def map2[EE >: E,B,C](b: EitherM[EE,B])(f: (A,B) => C): EitherM[EE, C] = {
    for { a <- this; bb <- b} yield f(a,bb)
  }

}

case class LeftM[+E](value:E) extends EitherM[E,Nothing]

case class RightM[+A](value:A) extends EitherM[Nothing,A]