package com.mimu.fpinscala

/**
  * 6.10
  */



object StateM {
  def unit[S, A](a:A): StateM[S,A] = StateM(s => (a, s))
  def sequence[S,A](fs:List[StateM[S,A]]): StateM[S,List[A]] = {
    fs.foldRight(unit[S,List[A]](List[A]()))((x, y) => x.map2(y)((a,b) => a :: b))
  }
  def get[S]: StateM[S,S] = StateM(s => (s,s))
  def set[S](s:S):StateM[S,Unit] = StateM(_ => ((), s))
  def modify[S](f:S => S): StateM[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

case class StateM[S, +A](run: S => (A,S)) {
  def flatMap[B](g: A => StateM[S,B]): StateM[S,B] = {
    StateM(s => {
      val (i,r) = run(s)
      g(i).run(r)
    })
  }
  def map[B](f:A => B): StateM[S,B] =
    this.flatMap[B] (
      a => {
        StateM.unit[S,B](f(a))
      }
    )
  def map2[B,C](rb: StateM[S,B])(f: (A,B) => C): StateM[S,C] =
    this.flatMap{ (a:A) =>
      rb.map{ (b:B) =>
        f(a,b)
      }
    }

}


