package com.mimu.fpinscala.chapter4test


sealed trait OptionM[+A]{

  /**
    * 4.1
    */
  def map[B](f: A => B): OptionM[B] = this match {
    case s:SomeM[A] => SomeM(f(s.get))
    case NoneM => NoneM
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case SomeM(a) => a
    case NoneM => default
  }

  def flatMap[B](f: A => OptionM[B]): OptionM[B] = this match {
    case SomeM(a) => f(a)
    case NoneM => NoneM
    //    map(f) getOrElse NoneM
  }

  def orElse[B >: A](ob: => OptionM[B]): OptionM[B] = {
    map(SomeM(_)) getOrElse ob
  }

  def filter(f: A => Boolean): OptionM[A] = {
//        case s:SomeM[A] => if (f(s.get)) SomeM(s.get) else NoneM
//        case NoneM => NoneM
    val ff = (a:A) => if (f(a)) SomeM(a) else NoneM
    flatMap(ff)
  }

}

case class SomeM[+A](get: A) extends OptionM[A]

case object NoneM extends OptionM[Nothing]

