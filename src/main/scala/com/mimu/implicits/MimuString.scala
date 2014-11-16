package com.mimu.implicits

/**
 * Created by mm on 15/03/2014.
 */

case class Delimiters(left: String, right: String)


class MimuString(val str:String) {
  def countMs = str.toUpperCase().filter(_ == 'M').size
  def withDelims(implicit delims:Delimiters) = delims.left + str + delims.right
}

object MimuString {
  def apply(str:String) = new MimuString(str)
  implicit def string2MiloszString(str:String) = new MimuString(str)
  implicit val d = Delimiters("<<<", ">>>")
}
