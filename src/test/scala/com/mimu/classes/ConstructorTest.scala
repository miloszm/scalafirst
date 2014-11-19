package com.mimu.classes

import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm.
 */
class ConstructorTest {

  /**
   * 4.1 SC Creating a Primary Constructor
   */
  @Test
  def basicPrimaryConstructorTest(): Unit = {
    class Person(var firstName:String, var lastName:String) {
      println("the constructor begins")

      /**
       * class fields
       */
      private val HOME = System.getProperty("user.home")
      var age = 0

      /**
       * methods
       */
      override def toString = s"$firstName $lastName is $age years old"
      def printHome: Unit = {
        println(s"HOME = $HOME")
      }
      def printFullName(): Unit = {
        println(this)
      }

      printHome
      printFullName
      println("still in the constructor")
    }

    val p = new Person("Max", "Blum")
    assertNotNull(p)
    p.firstName = "Mitch"
    println(p.firstName)
    p.age = 77
    println(p.age)
    p.age_$eq(78)
    println(p.age)
  }

  /**
   * 4.2 SC Controlling the Visibility of Constructor Fields
   */
  @Test
  def basicVisibilityTest(): Unit = {
    /**
     * 1 - public var
     */
    class Person1(var name:String){}
    val p1 = new Person1("malvin1")
    p1.name = "malvin11"
    println(p1.name)

    /**
     * 2 - public val
     */
    class Person2(val name:String){}
    val p2 = new Person2("malvin2")
    //p2.name = "malvin11"
    println(p2.name)

    /**
     * 3 - private val
     */
    class Person3(name:String){ // becomes a private val
      def someMethod():Unit = {
        //name = "malvin333"
        println(name)
      }
    }
    val p3 = new Person3("malvin3")
    //p3.name = "malvin33"
    //println(p3.name)
    p3.someMethod

    /**
     * 4 - private val - prevents getter and setter from being generated
     */
    class Person4(private val name:String){
      def someMethod():Unit = {
        //name = "malvin444"
        println(name)
      }
    }
    val p4 = new Person4("malvin4")
    //p4.name = "malvin44"
    //println(p4.name)
    p4.someMethod

    /**
     * 5 - private var
     */
    class Person5(){
      private var name:String = _
      def someMethod():Unit = {
        name = "malvin555"
        println(name)
      }
    }
    val p5 = new Person5()
    //p5.name = "malvin55"
    //println(p5.name)
    p5.someMethod

    /**
     * 6 - public var
     */
    class Person6(){var name:String = _}  // '_' inserts null here
    val p6 = new Person6()
    p6.name = "malvin66"
    println(p6.name)

    /**
     * 7 - case class
     */
    case class Person7(name:String)
    val p7 = Person7("malvin7")
    //p7.name = "malvin77"
    println(p7.name)

  }

  /**
   * 4.3 SC Defining Auxiliary Constructors
   */
  @Test
  def testAuxiliaryConstructors_NormalClasses(): Unit = {
    class Person(var name:String, var age:Int){
      def this(name:String){
        this(name, 70)
      }
      override def toString() = s"$name $age"
    }
    val p = new Person("malvin")
    println(p)
  }

  /**
   * 4.3 SC Defining Auxiliary Constructors
   *
   * NOTE - object for a case class needs to be declared before the case class
   * otherwise there is a compiler error that the object already exists (as
   * generated for the case class). So here order matters.
   * INTERESTING
   */
  @Test
  def testAuxiliaryConstructors_CaseClasses(): Unit = {
    object Person {
      def apply(name:String):Person = new Person(name)
    }
    case class Person(var name:String, var age:Int){
      def this(name:String){
        this(name, 70)
      }
      override def toString() = s"$name $age"
    }
    val p1 = Person("malvin")
    println(p1)
    /**
     * to see if generated object works
     */
    val p2 = Person("malvin", 77)
    println(p2)
    assertNotNull(p2)
    /**
     * yep, generated object works and it is enriched with my object - interesting
     * yet, if I did it after the case class, it would not compile
     */
  }

  /**
   * 4.4 SC Private Primary Constructor
   */
  @Test
  def testPrivatePrimaryConstructor(): Unit = {
    
  }


}
