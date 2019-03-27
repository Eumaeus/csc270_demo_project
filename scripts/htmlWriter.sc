
import scala.io.Source
import java.io._
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.seqcomp._
import edu.furman.classics.citealign._

val demoLib:String = "pope_iliad.cex"

def loadLibrary(fp:String = demoLib):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String = "../iliad_alignment/iliad_pope.txt"):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveString(s:String, filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}

// Convert an Int to a Roman Numeral
def toRoman(value: Int): String = {
"M" * (value / 1000) +
  ("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM").productElement(value % 1000 / 100) +
  ("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC").productElement(value % 100 / 10) +
  ("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX").productElement(value % 10)
}

// Convert a String that is a Roman Numeral to an Int
def fromRoman(s: String) : Int = {
	try {
		val numerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

		s.toUpperCase.map(numerals).foldLeft((0,0)) {
		  case ((sum, last), curr) =>  (sum + curr + (if (last < curr) -2*last else 0), curr) }._1
	} catch {
		case e:Exception => throw new Exception(s""" "${s}" is not a valid Roman Numeral.""")
	}
}

def printCorpus(c:Corpus):Unit = {
	println("------")
	for (n <- c.nodes) {
		// Use either this line:
		val thisCitation:String = n.urn.toString
		// or this line:
		//val thisCitation:String = n.urn.passageComponent.toString
		val thisText:String = n.text
		println(s"${thisCitation} :: ${thisText}")
	}	
	println("------")
}

lazy val lib = loadLibrary()
lazy val tr = lib.textRepository.get
lazy val popeCorpus = tr.corpus

// I'm lazy
def u(passage:String):CtsUrn = {
	val baseUrl:String = "urn:cts:fufolio:pope.iliad.fu2019:"
	CtsUrn(s"${baseUrl}${passage}")
}

def whichBook(u:CtsUrn):String = {
	if (u.passageComponent.size > 0) {
		u.collapsePassageTo(1).passageComponent
	} else {
		"1–24"
	}
}

// Getting labels for a URN
val groupName:String = tr.catalog.groupName(u(""))
val workTitle:String = tr.catalog.workTitle(u(""))
val versionLabel:String = tr.catalog.versionLabel(u(""))

// Chunked by 25
val chunk25:Vector[Corpus] = {
	// make Vectors with 25 passages in each
	val nodeChunks:Vector[Vector[CitableNode]] = popeCorpus.nodes.sliding(25,25).toVector
	// turn each of those into a Corpus
	val corpChunks:Vector[Corpus] = {
		nodeChunks.map( nodeVec => Corpus(nodeVec) )
	}
	// return that as a value
	corpChunks
}

// Chunked by Stanza, one way (wich will take forever):
val stanzas:Vector[Corpus] = {
	// get all URNs
	val allUrns:Vector[CtsUrn] = popeCorpus.urns
	// get only stanza-level URNs
	val stanzaUrns:Vector[CtsUrn] = allUrns.map(_.collapsePassageBy(1)).distinct
	// turn each stanza into a Corpus of verses from that stanza
	val stanzaCorps:Vector[Corpus] = {
		stanzaUrns.map(su => { popeCorpus ~~ su } )
	}
	// return that as a value
	stanzaCorps
}
