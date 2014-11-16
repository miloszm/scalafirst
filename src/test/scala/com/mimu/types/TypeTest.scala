package com.mimu.types

import org.junit.Assert._
import org.junit.Test

/**
 * Created by mm.
 */
class TypeTest {

  @Test
  def basicTypeTest(): Unit = {

    case class MyClass(val x:Int)

    val i1:MyClass = MyClass(3)

    type A = MyClass

    val i2:A = MyClass(4)

    /**
     * Section 29.6 PiS p.648
     * The ".type" on the end means that this is a singleton type.
     * A singleton type is very specific and holds only one object, whichever
     * object is refered to by i1.
     */
    val i3:i1.type = i1
    // i3 = i2 // this won't compile as the type of i3 is a singleton type i1.type

    /**
     * The following does not compile:
     */
    //val i4:i2.type = i1

    println(i2)
    println(MyClass(5))
    println(i3)

    assertEquals(i1,i3)

  }

  /**
   * Singleton types - 18.1 SftI
   */
  @Test
  def testSingletonTypes_Problem():Unit = {
    class Doc {
      def setTitle(title:String) = { this }
      def setAuthor(title:String) = { this }
    }

    val doc = new Doc
    doc.setTitle("Goto considered harmful").setAuthor("Hoare")

    class Book extends Doc {
      def addChapter(title:String) = {this}
    }

    val book = new Book

    /**
     * the following causes compilation error
     */
    //book.setTitle("Emma").addChapter("1")

  }

  /**
   * Singleton types - 18.1 SftI
   * Adding this.type as return type will make sure correct type is returned for subclasses
   */
  @Test
  def testSingletonTypes_Solution():Unit = {
    class Doc {
      def setTitle(title:String):this.type = { this }
      def setAuthor(title:String):this.type = { this }
    }

    val doc = new Doc
    doc.setTitle("Goto considered harmful").setAuthor("Hoare")

    class Book extends Doc {
      def addChapter(title:String) = {this}
    }

    val book = new Book

    /**
     * the following causes compilation error
     */
    book.setTitle("Emma").addChapter("1")

  }

  /**
   * Singleton types - 18.1 SftI
   * Passing object instance
   */
  @Test
  def testPassingObjectInstance_Problem():Unit = {
    object Title

    class Doc {
      /**
       * line below won't compile as Title is an object, not a class
       */
      //def setTitle(obj:Title):this.type = {this}
    }
  }

  /**
   * Singleton types - 18.1 SftI
   * Passing object instance
   */
  @Test
  def testPassingObjectInstance_Solution():Unit = {
    object Title

    class Doc {
      /**
       * line below compiles
       */
      def setTitle(obj:Title.type):this.type = {this}
    }
  }

  /**
   * Type projections - 18.2 SftI
   */
  @Test
  def testTypeProjection():Unit = {
    class Network {
      class Member(val v:Int)
    }
    val n1 = new Network
    val n2 = new Network

    object SomeObj {
      /**
       * nested class isn't a single type but has a separate type for each instance
       */
      def acceptMembers1(m:n1.Member) = this
      def acceptMembers2(m:n2.Member) = this

      /**
       * we want to declare method which will accept any Member class instance
       */
      def acceptMembers3(m:Network#Member) = this
    }
  }

  /**
   * type paths - 18.3 SftI
   */
  @Test
  def testTypePaths(): Unit = {
    val v = com.mimu.types.Abc.xyz
    assertEquals(5,v)
  }

  /**
   * type alias - 18.4 SftI
   */
  @Test
  def testTypeAlias(): Unit = {
    import scala.collection.mutable._
    type MyType = ArrayBuffer[String]
    val v:MyType = new MyType
    assertNotNull(v)
  }

  /**
   * type alias - 18.4 SftI
   */
  @Test
  def testTypeAliasInAbstractClasses(): Unit = {
    abstract class Man {
      type SomeResultType
      def obtain: SomeResultType
    }
    class Worker extends Man {
      override type SomeResultType = String
      override def obtain: SomeResultType = new SomeResultType
    }
    val w = new Worker
    assertNotNull(w)
  }

  /**
   * structural types - 18.5 SftI
   */
  @Test
  def testStructuralType: Unit = {

    class Printer {
      def print(o: {def toPrint():String}): String ={
        o.toPrint();
      }
    }

    class Printee {
      def toPrint():String = {
        return "printee"
      }
    }

    class NonPrintee{}

    val p = new Printer
    assertEquals("printee", p.print(new Printee))

    /**
     * the following won't compile because it does not have a method toPrint
     */
    //p.print(new NonPrintee)

  }

  /**
   * compound types - 18.6 SftI
   */
  @Test
  def testCompoundType: Unit = {

    class Printer {
      def print(o: PrinteeTrait with PrinteeTrait2 {def toPrint():String}): String ={
        o.toPrint();
      }
    }

    trait PrinteeTrait {}
    trait PrinteeTrait2 {}

    class Printee extends PrinteeTrait with PrinteeTrait2 {
      def toPrint():String = {
        return "printee"
      }
    }

    class PrinteeSimple extends PrinteeTrait /*with PrinteeTrait2*/ {
      def toPrint():String = {
        return "printeeSimple"
      }
    }

    val p = new Printer
    assertEquals("printee", p.print(new Printee))

    /**
     * won't compile if PrinteeSimple does not inherit trait PrinteeTrait2
     */
    //assertEquals("printeeSimple", p.print(new PrinteeSimple))

  }

  /**
   * compound types - 18.6 SftI
   */
  @Test
  def testCompoundType2: Unit = {

    class Printer {
      def print(o: Printee with PrinteeTrait {def toPrint():String}): String ={
        o.toPrint();
      }
    }

    trait PrinteeTrait {}

    class Printee extends PrinteeTrait {
      def toPrint():String = {
        return "printee"
      }
    }

    class PrinteeSimple /*extends PrinteeTrait*/{
      def toPrint():String = {
        return "printeeSimple"
      }
    }

    val p = new Printer
    assertEquals("printee", p.print(new Printee))

    /**
     * won't compile if PrinteeSimple does not inherit trait PrinteeTrait
     */
    //assertEquals("printeeSimple", p.print(new PrinteeSimple))

  }

  /**
   * infix types - 18.7 SftI
   */
  @Test
  def testInfixType():Unit = {
    type x[A,B] = (A,B)

    def createTuple:String x Int = ("abc", 15)
    def createTuple2:(String,Int) = ("abcd", 16)

    printf(createTuple.toString())
    printf(createTuple2.toString())

    assertEquals(("abc", 15), createTuple)
    assertEquals(("abcd", 16), createTuple2)
  }

  /**
   * existential types - 18.8 SftI
   */
  @Test
  def testExistentialType():Unit = {

    val a:Array[A] forSome { type A <: AnyRef} = new Array[String](3)
    assertNotNull(a)

  }

}


object Abc {
  val xyz = 5
}

