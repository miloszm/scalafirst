package com.mimu.fpinscala.ch6

import com.mimu.fpinscala.{RNG, SimpleRNG}

import scala.annotation.tailrec

object PureFunctions extends App {

  /**
    * 6.1
    */
  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (nn,rng1) = rng.nextInt
    val (n,rng2:RNG) = if (nn == Int.MinValue) nonNegativeInt(rng1) else (nn,rng1)
    (Math.abs(n), rng2)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

  /**
    * 6.2
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    val d = i.toDouble
    (d/Int.MaxValue+1,r)
  }

  /**
    * 6.3
    */
  def intDouble(rng:RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d,r2) = double(r)
    ((i,d),r2)
  }

  /**
    * 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case n => {
      val (i,r) = rng.nextInt
      val (tail, _) = ints(n-1)(r)
      (i :: tail, r)
    }
  }
  def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(l:List[Int], count:Int, rng:RNG):(List[Int], RNG) = {
      if (count == 0){
        (l, rng)
      }
      else {
        val (i,r) = rng.nextInt
        go(i :: l, count-1, r)
      }
    }
    go(List(),count,rng)
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a:A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f:A => B): Rand[B] = {
    rng => {
      val (i,r) = s(rng)
      (f(i),r)
    }
  }

  /**
    * 6.5
    */
  def double2(rng: RNG): (Double, RNG) = {
    val m = map(nonNegativeInt)(i => (i.toDouble/(Int.MaxValue+1)))
    m(rng)
  }
  val double3: Rand[Double] = map(nonNegativeInt)(i => (i.toDouble/(Int.MaxValue+1)))

  /**
    * 6.6
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, r) = ra(rng)
      val (b, r2) = rb(r)
      (f(a, b), r2)
    }
  }

  /**
    * 6.7
    */
  def sequence[A](fs:List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def go(fs:List[Rand[A]], r:Rand[List[A]]): Rand[List[A]] = fs match {
      case Nil => r
      case x :: tail => go(tail, map2(x, r)((a, b) => a :: b))
    }
    go(fs, unit(List()))
  }
  def sequence2[A](fs:List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((x, y) => map2(x,y)((a,b) => a :: b))
  }
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    val rand = sequence(List.fill(count)(int))
    rand(rng)
  }
  def ints3(count: Int)(rng: RNG): (List[Int], RNG) = sequence2(List.fill(count)(int))(rng)

  /**
    * 6.8
    */
  def flatMap[A,B](f:Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (i,r) = f(rng)
      g(i)(r)
    }
  }
  def nonNegativeLessThan(n:Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) {
        unit(mod)
      }
      else {
        nonNegativeLessThan(n)
      }
    }

  /**
    * 6.9
    */
  def mapViaFlatMap[A,B](s: Rand[A])(f:A => B): Rand[B] =
    flatMap(s){
      i => {
        unit(f(i))
      }
    }
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra){ a =>
      map(rb){ b =>
        f(a,b)
      }
    }



  val srng = SimpleRNG(1471)
  println(nonNegativeInt(srng))
  println(intsTailRec(10)(srng))
  println(double3(srng))
  println(ints2(7)(srng))
  println(ints3(7)(srng))
  println(nonNegativeLessThan(50)(srng))
}
