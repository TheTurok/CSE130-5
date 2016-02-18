import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] = {
    val fileWords = scala.io.Source.fromFile(file)
    val fileIt = fileWords.getLines().toIterator
    fileIt.map(_.toLowerCase)
    }
  
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = 
  {
    var hashBrowns = new HashMap[B, Int]

    for ( x <- xs ) 
      hashBrowns +=  f(x) -> ( hashBrowns.getOrElse(f(x), 0) + 1)
    
    hashBrowns
  }
 
  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = 
  {
    val theFile = scala.io.Source.fromFile(file)
    val fileIt = theFile.getLines().toIterator
    groupFreq[String, Int] (fileIt, (x => x.length))
  }

  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars   = scala.io.Source.fromFile(file).getLines().toIterator.flatMap(_.toLowerCase)
    val grouper = (x:Char) => x
    groupFreq(chars, grouper) 
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = 
  {
    val theFile = scala.io.Source.fromFile(file).getLines.toIterator
    theFile.toList.filter(_.length() == size).toIterator
  }

  def wordsWithAllVowels(file: String): Iterator[String] = 
  {
    val theFile = apply(file)
    val fileString = theFile.toString
    theFile.filter(containVowel)
  } 
 
  def wordsWithNoVowels(file: String): Iterator[String] = 
  {
    val theFile = apply(file)
    val fileString = theFile.toString
    theFile.filterNot(noVowel) 
  }


  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = {
    val reStr = re.toString
    val fileIt = scala.io.Source.fromFile(file).getLines().toIterator.map(_.toLowerCase)
    fileIt.filter(_ matches(reStr))
}

   def containVowel (x:String):Boolean = {
     var a = false
     var e = false
     var i = false
     var o = false
     var u = false
     for(w <- x){
       if(w == 'a')
         a = true
       if(w == 'e')
         e = true
         if(w == 'i')
         i = true
       if(w == 'o')
         o = true
       if(w == 'u')
         u = true
     }
     a&&e&&i&&o&&u
   }


   def noVowel (x:String):Boolean = {
     var some = false
     for(w <- x){
       if(w == 'a' || w == 'e' || w == 'i' || w == 'o' || w == 'u')
         some = true
     }
     some
   } 


  }


// vim: set ts=2 sw=2 et:

