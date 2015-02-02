import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.FileNotFoundException
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class kwicTest extends FunSuite with BeforeAndAfter{
  
  
  test("read in stopwords") { 
    val stopwords = kwic.stop_words
    assert(stopwords.contains("furthermore"))
    assert(stopwords.contains("after"))
    assert(stopwords.contains("same"))
  }
  
  test("read files"){
    val stopwords = kwic.stop_words
    val wordmap = kwic.readfile("test.txt", stopwords)
    assert(wordmap.contains("cancel"))
    assert(wordmap("cancel").contains(Tuple2(1, "Trait Assertions also provides methods that allow you to cancel a test. ")))
    assert(wordmap("cancel").contains(Tuple2(2, "You would cancel a test if a resource required by the test was unavailable. ")))
    assert(wordmap.contains("test"))
    assert(wordmap("test").length === 7)
  }
  
  test("output files"){
    val stopwords = kwic.stop_words
    val file = "test.txt"
    val wordmap = kwic.readfile(file, stopwords)
    kwic.outputfile(file, wordmap)
    val filename = "kwic_index.txt"
    val filereader = new ArrayBuffer[String]()
    for (line <- Source.fromFile(filename).getLines()) {
       filereader += line
    }
    assert(filereader.contains("%d\t%30s\t%s\t%s".format(1, "des methods that allow you to ", "cancel", " a test.  ")))
    assert(filereader.contains("%d\t%30s\t%s\t%s".format(3, " be online, and it isn't, the ", "test", " could be canceled to indicate")))
  }
}