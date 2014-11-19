package com.mimu.classes

import org.junit.Test
import org.junit.Assert._

/**
 * Created by mm.
 */
class ClassesAndPropertiesTest {

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
  def testPrivatePrimaryConstructor_IeSingleton(): Unit = {

    class Person private (name:String)

    //val p = new Person() // won't compile - constructor is private
    //println(p)

    object Person {
      val person = new Person("malv")
      def getInstance = person
    }

    val p = Person.getInstance
    println(p)

    /**
     * in many cases, if you need a singleton, just use object
     */

  }

  /**
   * 4.5 SC Default Parameters
   */
  @Test
  def testConstructorDefaultParameters(): Unit = {

    class Person(name: String = "Malvin"){
      override def toString() = s"name=$name"
    }

    val p = new Person
    println(p)

  }

  /**
   * 4.5 SC Default Parameters - named parameters
   */
  @Test
  def testConstructorDefaultNamedParameters(): Unit = {

    class Person(val name: String = "Malvin", val age: Int = 77){
      override def toString() = s"name=$name age=$age"
    }

    val p = new Person(age = 88)
    println(p)
    assertEquals(88, p.age)

    val p2 = new Person(age = 37, name = "Lorena")
    println(p2)
    assertEquals(37, p2.age)

  }

  /**
   * 4.6 SC Overriding Default Accessors and Mutators
   */
  @Test
  def testOverridingDefaultAccessorsAndMutators(): Unit = {
    class Person(private var _name: String) {
      def name = _name
      def name_= (aName: String){ _name = aName }
      override def toString() = s"name=$name"
    }

    val p = new Person("Lorena")
    p.name = "Kasia"
    println(p)
    assertEquals("Kasia", p.name)

  }

  /**
   * 4.7 SC
   */
  @Test
  def testObjectPrivateFields(): Unit = {
    class Person {
      private/*[this]*/ var age: Int = _
      def setAge(age:Int){ this.age = age}
      def older(that:Person):Boolean = this.age > that.age // wont compile for private[this], will compile for just private
    }
  }

  /**
   * 4.8 SC
   */
  @Test
  def testLazyField(): Unit = {
    class Lorena {
      lazy val text = io.Source.fromFile("/etc/hosts").getLines.foreach(println)
    }
    val l = new Lorena

    println("lazy text not intialized yet, waiting...")
    Thread.sleep(2000)

    l.text
  }

  /**
   * 4.9 SC
   */
  @Test
  def testInitializing(): Unit = {
    case class Address(val a:String)
    class Lorena {
      var address = None: Option[Address]   // both styles work
      var address2: Option[Address] = None  // I like this style better
    }

  }

  /**
   * 4.10 SC - Handling constructor parameters when extending a class
   */
  @Test
  def testExtending(): Unit = {
    case class Address(val a:String)
    class Person(var name: String, var address:Address) {
      override def toString() = s"name=$name address=$address"
    }
    /**
     * do not use val or ver for the inherited constructor params
     * this way you avoid generation of accessors in the subclass
     */
    class Employee(name: String, address: Address, var yearJoined: Int) extends Person(name, address){
      override def toString() = super.toString() + s" yearJoined=$yearJoined"
    }

    val lorena = new Employee("Lorena", Address("Pinto Way 1"), 2013)
    println(lorena)
  }

  /**
   * 4.11 SC - Calling a superclass constructor
   */
  @Test
  def testCallingASuperclassConstructor(): Unit = {
    class Person(var name: String, var age:Int){

      def this(name:String){
        this(name, 0)
      }

    }
    /**
     * either extends Person(name) or extends Person(name, 0) are ok
     */
    class Employee(name:String) extends Person(name, 0){
      /**
       * NOTE - auxiliary constructors here can't call superclass constructor
       */
      def this() {
        this("")
      }
    }

  }

  /**
   * 4.12 SC - When to use an abstract class
   */
  @Test
  def test_SC_4_12(): Unit = {
    /**
     * trait's don't allow constructor parameters
     */
    //trait Castle(name:String) // won't compile
    abstract class Castle(name:String){
      def portcullisDown    // no body makes it abstract
    }

  }

  /**
   * 4.13 SC - Defining properties in an abstract base class (or trait)
   */
  @Test
  def test_SC_4_13(): Unit = {

  }

}
