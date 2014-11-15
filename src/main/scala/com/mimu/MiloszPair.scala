package com.mimu

/**
 * Created by mm on 15/03/2014.
 */
case class MiloszPair[A : Ordering ](val first: A, val second: A){
  //def smaller(implicit ord: Ordering[A]) = if (ord.compare(first,second) < 0) first else second
  def smaller() = if (implicitly[Ordering[A]].compare(first,second) < 0) first else second
}
