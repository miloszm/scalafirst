package com.mimu.neophyte.pathdependent

/**
  * from neophyte's guide to scala
  */
object PathDependentTypesBefore {

  object Franchise {
    case class Character(name: String, franchise: Franchise)
  }

  class Franchise(name: String) {
    import Franchise.Character
    def createFanFiction(
      lovestruck: Character,
      objectOfDesire: Character): (Character, Character) = {
        require(lovestruck.franchise == objectOfDesire.franchise, "must be the same franchise")
        (lovestruck, objectOfDesire)
      }
  }

  val starTrek = new Franchise("Star Trek")
  val starWars = new Franchise("Star Wars")

  val quark = Franchise.Character("Quark", starTrek)
  val jadzia = Franchise.Character("Jadzia Dax", starTrek)

  val luke = Franchise.Character("Luke Skywalker", starWars)
  val yoda = Franchise.Character("Yoda", starWars)


  class A {
    class B
    var b: Option[B] = None
  }

  val a1 = new A
  val a2 = new A
  val b1 = new a1.B
  val b2 = new a2.B
  a1.b = Some(b1)
  //a2.b = Some(b1) // does not compile

}
