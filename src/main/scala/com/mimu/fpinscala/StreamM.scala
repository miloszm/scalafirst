package com.mimu.fpinscala

import StreamM._


/**
  * 5.1 5.2
  */
sealed trait StreamM[+A] {
  def headOption: Option[A] = this match {
    case EmptyM => None
    case ConsM(h,t) => Some(h())
  }
  def toList: List[A] = this match {
    case EmptyM => List()
    case ConsM(h,t) => h() :: t().toList
  }
  def take(n:Int): StreamM[A] = this match {
    case EmptyM => empty
    case s if (n == 0) => empty
    case ConsM(h,t) => cons(h(), t().take(n-1))
  }
  def drop(n:Int): StreamM[A] = this match {
    case ConsM(_,t) if (n > 0) => t().drop(n-1)
    case _ => this
  }
  def takeWhile(p: A => Boolean):StreamM[A] = this match {
    case ConsM(h,t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }
}

case object EmptyM extends StreamM[Nothing] {
  override def toString: String = "<empty>"
}

case class ConsM[+A](h: () => A, t: () => StreamM[A]) extends StreamM[A] {
  override def toString: String = s"h=${h()} t=${t().toString}"
}

object StreamM {
  def cons[A] (hd: => A, tl: => StreamM[A]): StreamM[A] = {
    lazy val head = hd
    lazy val tail = tl
    ConsM(() => head, () => tail)
  }
  def empty[A]: StreamM[A] = EmptyM
  def apply[A](as: A*): StreamM[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
}

