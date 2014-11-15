import scala.collection.mutable.ArrayBuffer
import scala.util.Random

val x = 4
val y = x.*(x)
val z = x

var b = ArrayBuffer(1,4,2,6)

def sortFun(a:Int,b:Int):Boolean = {
  a < b
}

//val bSorted = b.sorted(sortFun)

val bSorted2 = b.sortWith(_ < _)

val n = 10
val c = for {i <- 0 until n} yield Random.nextInt(n)
val d = c.toList

val a = Array(1,2,3,4,5)

//for (i <- (0 until a.length) if (i % 2 == 0) && (i + 2 < a.length) ) {
//  val x = a(i+1)
//  a(i+1) = a(i)
//  a(i) = x
//}

a

val aa = (for (i <- (0 until a.length)) yield {
  if (i % 2 == 0 && i + 1 < a.length){
      a(i+1)
  }
  else if (i % 2 != 0){
      a(i-1)
  }
  else {
    a(i)
  }
}).toList

val f = Array(-1,4,-3,2,-2,6)

for ( r <- 0 to 1; e <- f; if ((r == 0 && e < 0) || (r == 1 && e > 0) )) yield {e}

val g = Array(1.1,1.2,1.3,1.3).distinct

g


g.sum/g.length





