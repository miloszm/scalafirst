package com.mimu.neophyte.pathdependent

/**
  * from neophyte's guide to scala
  */
object PathDependentTypesAfter {

  object Franchise {
  }

  class Franchise(name: String) {
    case class Character(name: String)
    def createFanFictionWith(
      lovestruck: Character,
      objectOfDesire: Character): (Character, Character) = {
        (lovestruck, objectOfDesire)
      }
  }

  val starTrek = new Franchise("Star Trek")
  val starWars = new Franchise("Star Wars")

  val quark = starTrek.Character("Quark")
  val jadzia = starTrek.Character("Jadzia Dax")

  val luke = starWars.Character("Luke Skywalker")
  val yoda = starWars.Character("Yoda")

  starTrek.createFanFictionWith(lovestruck = quark, objectOfDesire = jadzia)
  starWars.createFanFictionWith(lovestruck = luke, objectOfDesire = yoda)
//  starTrek.createFanFictionWith(lovestruck = jadzia, objectOfDesire = luke)// does not compile


}
