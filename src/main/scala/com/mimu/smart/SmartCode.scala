package com.mimu.smart

/**
  * from neophyte's guide to scala
  */
object SmartCode extends App {

  val lists = List(List(1,2,3),List.empty,List(1,2))

  def nonEmptyLists = for {
    list @ head :: _ <- lists
  }
  yield list

  println(nonEmptyLists)


  val wordFrequencies = List( ("habitual", 6), ("and", 56), ("consuetudinary", 2), ("additionally", 27), ("homely", 5), ("society", 13))

  def wordsWithoutOutliers2(wordFrequencies: Seq[(String, Int)]): Seq[String] =
    wordFrequencies.filter(wf => wf._2 > 3 && wf._2 < 25).map(_._1)

  def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
    wordFrequencies.filter{case(_,f) => f > 3 && f < 25}.map{case(w,_) => w}


  println(wordsWithoutOutliers(wordFrequencies))

  val pf: PartialFunction[(String,Int),String] = {
    case (word,freq) if freq > 3 && freq < 25 => word
  }

  println(wordFrequencies.collect(pf))

  println(wordFrequencies.collect{case (w,f) if f > 3 && f < 25 => w })

}
