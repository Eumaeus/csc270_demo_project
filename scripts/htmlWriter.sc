
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

val demoLib:String = "pope_iliad.cex"

def loadLibrary(fp:String = demoLib):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String = "pope_iliad.txt"):Vector[String] = {
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

// Chunked by Stanza, one way (which will take forever, 1347 seconds tested):
lazy val stanzas:Vector[Corpus] = {
	// get all URNs
	val allUrns:Vector[CtsUrn] = popeCorpus.urns
	// get only stanza-level URNs
	val stanzaUrns:Vector[CtsUrn] = allUrns.map(_.collapsePassageBy(1)).distinct
	// turn each stanza into a Corpus of verses from that stanza
	val stanzaCorps:Vector[Corpus] = {
		stanzaUrns.map(su => { 
			popeCorpus ~~ su 
		})
	}
	// return that as a value
	stanzaCorps
}

// Chunked by Stanza, another way ( < 1 second)
lazy val stanzas2:Vector[Corpus] = {
	// We need this, for this process only…
	import scala.collection.mutable.LinkedHashMap
	// we start with a Vector of CitableNodes from our corpus
	val v1:Vector[CitableNode] = popeCorpus.nodes
	// We zipWithIndex to capture their sequence	
	val v2:Vector[(CitableNode, Int)] = v1.zipWithIndex
	/* 
		We want to group these by the urn-values, discarding
		the right-hand element.

		_._1 is a CitableNode
		_._1.urn is the URN of the CitableNode
		_._1.urn.collapsePassageBy(1) gives us the stanza-level
	*/
	val v3:Vector[(CtsUrn, Vector[(CitableNode, Int)])] = {
		v2.groupBy( _._1.urn.collapsePassageBy(1) ).toVector
	}
	/*
	The complicated bit. groupBy does not preserve sequence.
	So we use the Int value
		above: in Vector[CtsUrn, Vector[(CitableNode, Int)]]
			or, when iterating,
		_._2.head._2
	to re-sort our groups
	*/
	//val v4 = LinkedHashMap(v3.toSeq.sortBy(_._2.head._2): _*)
	val v4 = LinkedHashMap(v3.sortBy(_._2.head._2): _*)

	// this removes the unnecessary index info
	val v5 = v4.mapValues(_ map (_._1)).toVector

	// turn Vector[Vector[CitableNodes]] into a Vector[Corpus]
	val corpusVec:Vector[Corpus] = v5.map( v => {
		val nodes:Vector[CitableNode] = v._2
		Corpus(nodes)
	})

	corpusVec
}
