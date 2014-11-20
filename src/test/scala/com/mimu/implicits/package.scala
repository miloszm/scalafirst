package com.mimu

/**
 * Created by miloszmuszynski on 20/11/2014.
 */
package object implicits {

  implicit class StringImplicitTestClass(val str: String) {
    def someMethod():String = {
      str + str
    }
  }

}
