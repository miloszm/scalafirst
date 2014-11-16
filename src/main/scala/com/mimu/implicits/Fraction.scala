package com.mimu

/**
 * Created by mm.
 */
class Fraction(val num:Int, val den:Int){

  def *(f:Fraction):Fraction = Fraction(num * f.num, den * f.den)

  override def toString():String = "(" + num + ", " + den + ")";

}

object Fraction {
  implicit def int2Fraction(n:Int) = new Fraction(n, 1)
  def apply(num:Int, den:Int):Fraction = new Fraction(num,den)
}

