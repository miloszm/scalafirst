package com.mimu.implicits

/**
 * Created by mm on 15/03/2014.
 */
case class MimuPair[A : Ordering ](val first: A, val second: A){
  def smaller() = if (implicitly[Ordering[A]].compare(first,second) < 0) first else second
}
