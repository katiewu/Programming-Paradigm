import scala.io.Source
import scala.collection.mutable.SortedSet
import scala.collection.mutable.ArrayBuffer
import java.io.FileNotFoundException
import java.io.IOException
import scala.collection.mutable.HashMap
import java.io._
import java.util.Formatter
object kwic {

  def main(args: Array[String]){
    val stopwords = stop_words
    val filelist = inputfiles
    for(file <- filelist){
      val wordmap = readfile(file, stopwords)
      outputfile(file, wordmap)	      
    }
    println("finish write kwic_index.txt")
  }

  def stop_words: SortedSet[String] = {
    val filename = "stop_words.txt";
    val stopwords = SortedSet[String]()
    println(Source.fromFile(filename))
    for (line <- Source.fromFile(filename).getLines()) {
      stopwords += line.toLowerCase
    }
    return stopwords
  }
  
  def inputfiles: ArrayBuffer[String] = {
    println("Enter filename:")
    var input = readLine
    val filelist = new ArrayBuffer[String]()
    while(input != ""){
      filelist += input
      println("Enter filename:")
      input = readLine
    }
    return filelist    
  }
  
  def readfile(file: String, stopwords: SortedSet[String]): HashMap[String, ArrayBuffer[(Int, String)]] = {
    try {
    	var count = 0;
    	val wordmap = HashMap[String, ArrayBuffer[(Int, String)]]();
	    for (line <- Source.fromFile(file).getLines()){
	      count = count + 1;
	      val words = line.split("[^'\\w]+")
	      for (word <- words){
	        val wordlower = word.toLowerCase
	        if((!stopwords.contains(wordlower))&&wordlower!=""){
	        	if(!wordmap.contains(wordlower)){
	        	  wordmap += (wordlower -> new ArrayBuffer[(Int, String)])
	        	}
	        	wordmap(wordlower) += new Tuple2(count, line)
	        }
	      }
	    }
	    return wordmap
	        
    } catch {
         case ex: FileNotFoundException =>{
            println("File does not exist.")
            return null
         }
         case ex: IOException => {
            println("IO Exception")
            return null
         }
    }
    
  }
  
  def outputfile(file:String, wordmap: HashMap[String, ArrayBuffer[(Int, String)]]){
    try{
      val output = "kwic_index.txt"
      val writer = new PrintWriter(new FileWriter(output, true))
      writer.write("%s\n".format(file))
      wordmap.toList.sortWith( (x,y) => x._1 < y._1 )  foreach {
	      case (key, value) => {
	        val list = value.toList.sorted.distinct
	        for(i <- list){
	          val slst = (i._2+" ").toLowerCase.split(key)
	          for(k<- 0 to (slst.length-2)){
	            var left = ""
	            var right = ""
	            if(slst(k)!=""){
	              left = slst(k).substring(math.max((slst(k).length-30), 0))
	            }
	            if(slst(k+1)!=""){
	              right = slst(k+1).substring(0, math.min(30, slst(k+1).length))
	            }	            
	            writer.write("%d\t%30s\t%s\t%s\n".format(i._1, left, key, right))
	          }          	          
	        }        
	      }
      }
      writer.write("\n")
      writer.close()
    }catch{
      case ex: FileNotFoundException => {
        println("Output File does not exist.")
      }
      case ex: IOException => {
        println("IO Exception")
      }
    }
    
    		
      
  }
}

