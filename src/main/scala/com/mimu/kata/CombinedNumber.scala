package com.mimu.kata

/**
  * Created by miloszmuszynski on 26/08/2016.
  */
object CombinedNumber {
  def getBiggest(ints: List[Int]): String = {

    perms(ints).maxBy(_.mkString.toInt).mkString

  }

  def perms(ints: List[Int]): List[List[Int]] = ints match {
      case Nil => List(List())
      case head :: tail => {
        for {
          p <- perms(tail)
          index <- 0 to tail.size
        }
        yield {
          insertAtPos(head, index, p)
        }
      }
    }


  def insertAtPos(i:Int, pos:Int, l:List[Int]): List[Int] = {
    val (x: List[Int], y: List[Int]) = l.splitAt(pos)
    x ::: (i :: y)
  }

}

