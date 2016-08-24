package com.mimu.fpinscala.ch5

import com.mimu.fpinscala.StreamM

/**
  * 5.1 5.2 5.3
  */
object Streams extends App {

  println(StreamM(1,2,3,4).take(2))
  println(StreamM(1,2,3,4).take(0))
  println(StreamM(1,2,3,4).take(4))
  println(StreamM(1,2,3,4).take(5))

  println(StreamM(1,2,3,4).drop(2))
  println(StreamM(1,2,3,4).drop(0))
  println(StreamM(1,2,3,4).drop(4))
  println(StreamM(1,2,3,4).drop(5))

}
