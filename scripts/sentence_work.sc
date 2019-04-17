
import scala.io.Source
import java.io._
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.seqcomp._
import edu.furman.classics.citealign._
import java.util.Calendar

/* Utilities */

val stopWords:Vector[String] = Vector("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than", "thou", "o'er", "thus", "thy", "yet", "thee", "shall")

def showMe(v:Any):Unit = {
	v match {
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveString(s:String, filePath:String = "", fileName:String = ""):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}

val splitters:String = """[\[\])(:·⸁.,·;;   "?·!–—⸂⸃]"""
val sentencePunct:String = """[.;?!]"""


def countSyllables(s:String):(String, Int) = {
	val dumbSyllabifier = """[^aeiou]*[aeiouy]+""".r
	val initialCount = dumbSyllabifier.findAllIn(s).size
	val modString:String = {
		if (initialCount == 1) {
			s
		} else {
			s.replaceAll("es?$","")
		}
	 }
	val syllables:Int = dumbSyllabifier.findAllIn(modString).size
	(s, syllables)
}

/* Project-specific CEX Stuff */

val myCexFile:String = "pope_iliad.cex"

lazy val lib = loadLibrary(myCexFile)
lazy val tr = lib.textRepository.get
lazy val popeCorpus = tr.corpus

/*
case class CaseInsensitiveString(s:String) {
	def equals(otherString:String):Boolean = {
		s.toLowerCase == otherString.toLowerCase
	}
	def == (otherString:String): Boolean = this.equals(otherString)
}
*/

/*
val v = Vector("Dogs","dOgs","dogs","cats","dormice")
val vcis = v.map(s => CaseInsensitiveString(s))
vcis.filter(_ == "DOGS")
*/
