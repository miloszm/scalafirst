package com.mimu.neophyte.pathdependent

/**
  * from neophyte's guide to scala
  */
object AbstractTypeMember extends App {

  object AwesomeDB {
    abstract class Key(name: String) {
      type Value
    }
  }

  import AwesomeDB.Key
  class AwesomeDB {
    import collection.mutable.Map
    val data = Map.empty[Key, Any]
    def get(key: Key): Option[key.Value] = data.get(key).asInstanceOf[Option[key.Value]]
    def set(key: Key)(value: key.Value): Unit = data.update(key, value)
  }

  trait IntValued extends Key {
    type Value = Int
  }

  trait StringValued extends Key {
    type Value = String
  }

  object Keys {
    val intValue = new Key("foo") with IntValued
    val stringValue = new Key("bar") with StringValued
  }

  val x = new AwesomeDB()
  x.set(Keys.intValue)(5)
  x.set(Keys.stringValue)("five")
  val r:Int = x.get(Keys.intValue).getOrElse(0)
  val r2:String = x.get(Keys.stringValue).getOrElse("zero")
  println(r)
  println(r2)

}
