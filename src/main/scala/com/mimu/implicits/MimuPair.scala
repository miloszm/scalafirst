package com.mimu.implicits

/**
 * Created by mm.
 */
case class MimuPair[A : Ordering ](val first: A, val second: A){
  def smaller() = if (implicitly[Ordering[A]].compare(first,second) < 0) first else second
}
