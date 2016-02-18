import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter
import java.io._
import scala.io.Source

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  
  def apply(line: String) : Entry = {
    val lineSplit = line.split(":")
    Entry(lineSplit(0),
      lineSplit(1),
      lineSplit(2).toInt,
      lineSplit(3).toInt,
      lineSplit(4),
      lineSplit(5),
      lineSplit(6))
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    val rev = w.reverse
    val theWord = s"""$w\n$rev"""
    theWord.lines
  }
  
  def transformCapitalize(w: String) : Iterator[String] = {
    if(w == "")
    {
      Iterator("")
    }
    else
    {
      val letter = w.head.toUpper
      val wList = List(w.head, letter)
      val wIt = wList.toIterator
      for(pre <- wIt; post <- transformCapitalize(w.tail))
        yield (pre + post)
   }
  }

  def transformDigits(w:String) : Iterator[String] = {
   if(w == "")
    {
      Iterator("")
    }
    else
    {
      val letter = w.head
      val lower = letter.toLower 
      val intList = lower match{
        case 'o' => Iterator(w, '0')
        case 'i' => Iterator(w, '1')
        case 'l' => Iterator(w, '1')
        case 'z' => Iterator(w, '2')
        case 'e' => Iterator(w, '3')
        case 'a' => Iterator(w, '4')
        case 's' => Iterator(w, '5')
        case 'b' => Iterator(w, '6', '8')
        case 't' => Iterator(w, '7')
        case 'g' => Iterator(w, '9')
        case 'q' => Iterator(w, '9')
        case  _  => Iterator(w) 
      }
      for(pre <- intList; post <- transformDigits(w.tail))
        yield (pre + post)
    }
   
   
   }

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {


}
  
 
 
 def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:

