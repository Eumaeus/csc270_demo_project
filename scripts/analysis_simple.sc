
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


/* Stop Words */

val stopWords:Vector[String] = Vector("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than", "thou", "o'er", "thus", "thy", "yet", "thee", "shall")
	
/* Utilities */

def showMe(v:Any):Unit = {
	v match {
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
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

val splitters:String = """[\[\])(:·⸁.,·;; "?·!–—⸂⸃]"""

/* Project-specific CEX Stuff */

val myCexFile:String = "pope_iliad.cex"

lazy val lib = loadLibrary(myCexFile)
lazy val tr = lib.textRepository.get
lazy val popeCorpus = tr.corpus

popeCorpus.ngramHisto(4,8) // 4-grams occuring more than 8 times

/* Make an "analytical exemplar" of your text */
/*
val newCorpus:Corpus = {
	val nodeVector:Vector[CitableNode] = popeCorpus.nodes.map(n => {
		val newUrn:CtsUrn = n.urn.addExemplar("analysis")
		val cleanText:String = n.text.replaceAll(splitters," ").replaceAll(" +"," ").toLowerCase
		CitableNode(newUrn,cleanText)	
	})
	Corpus(nodeVector)
}

newCorpus.ngramHisto(4,8) // 4-grams occuring more than 8 times
*/

/* Make another */
/*
val analysisCorpus:Corpus = {
	val nodeVector:Vector[CitableNode] = newCorpus.nodes.map(n => {
		val newUrn:CtsUrn = n.urn.dropExemplar.addExemplar("topics")
		val textVec:Vector[String] = n.text.split(splitters).toVector
		val removedStopWords:String = {
			textVec.filter( t => {
				stopWords.contains(t) == false
			}).mkString(" ")
		}
		CitableNode(newUrn,removedStopWords)	
	})
	Corpus(nodeVector)
}
*/







