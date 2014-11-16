package com.mimu.types

import org.junit.Test

/**
 * Created by mm.
 *
 * 18.11 of SftI - Dependency Injection
 */
class DepInjectionTest {

  @Test
  def basicDepInjectionTest():Unit = {

    trait Logger {
      def log(msg:String)
    }

    trait ConsoleLogger extends Logger {
      override def log(msg: String): Unit = println("x")
    }

    trait FileLogger extends Logger {
      override def log(msg: String): Unit = println("x")
    }

    trait Auth {
      this: Logger =>
      def login(id:String, password:String):Boolean
    }

    trait MockAuth extends Auth with ConsoleLogger {
      override def login(id: String, password: String): Boolean = false
    }

    trait App {
      this: Logger with Auth =>
        def appLogic(): Unit = {}
    }

    object MyApp extends App with FileLogger with MockAuth {

    }

    /**
     * better use "cake pattern" than the above
     */
  }

  /**
   * cake pattern
   */
  @Test
  def testCakePattern():Unit = {

    /**
     * logger component
     */
    abstract trait LoggerComponent {
      trait Logger {
        def log(msg:String)
      }
      val logger: Logger
      class FileLogger extends Logger {
        override def log(msg: String): Unit = println("x")
      }
    }

    /**
     * auth component
     */
    abstract trait AuthComponent {
      this:LoggerComponent =>
      trait Auth {
        def login(id:String, password:String):Boolean
      }
      val auth: Auth
      class MockAuth extends Auth {
        override def login(id: String, password: String): Boolean = false
      }

    }

    /**
     * configuration
     */
    object AppComponents extends LoggerComponent with AuthComponent {
      val logger = new FileLogger()
      val auth = new MockAuth()
    }

  }

}
