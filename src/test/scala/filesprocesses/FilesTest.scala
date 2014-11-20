package filesprocesses

import org.junit.Test
import org.junit.Assert._

import scala.io.{BufferedSource, Source}

/**
 * Created by mm.
 */
class FilesTest {

  /**
   * 12.1 SC - Open and read a text file
   */
  @Test
  def basicFileTest(): Unit = {
    val filename = "/etc/hosts"
    for (line <- Source.fromFile(filename).getLines()){
      //println(line)
    }

    val fileContents = Source.fromFile(filename).getLines.mkString
    println(fileContents)

    /**
     * file is left open
     */

    val bs:BufferedSource = Source.fromFile(filename)
    val fileContents2 = bs.getLines().mkString
    bs.close()

    /**
     * file is closed now
     */

  }

  /**
   * 12.1 SC - Open and read a text file
   */
  @Test
  def fileTest_Using_LoanPattern(): Unit = {

    object Control {
      def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
        try {
          f(resource)
        } finally {
          resource.close()
        }
    }

    Control.using(io.Source.fromFile("/etc/hosts")){ source => {
      for (line <- source.getLines) {
        println(line)
      }
    }}

  }


}
