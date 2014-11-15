package com.mimu

/**
 * Created by mm on 15/03/2014.
 */

case class Delimiters(left: String, right: String)


class MiloszString(val str:String) {
  def countMs = str.toUpperCase().filter(_ == 'M').size
  def withDelims(implicit delims:Delimiters) = delims.left + str + delims.right
}

object MiloszString {
  def apply(str:String) = new MiloszString(str)
  implicit def string2MiloszString(str:String) = new MiloszString(str)
  implicit val d = Delimiters("<<<", ">>>")
}
