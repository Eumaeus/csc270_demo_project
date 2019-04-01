
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

def saveString(s:String, filePath:String = "html/", fileName:String = "temp.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
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
//     note that these depend on the stuff defined above
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

// Chunk-by-citation
def chunkByCitation(c:Corpus, level:Int = 1):Vector[Corpus] = {
	// we start with a Vector of CitableNodes from our corpus
	val v1:Vector[CitableNode] = c.nodes
	// We zipWithIndex to capture their sequence	
	val v2:Vector[(CitableNode, Int)] = v1.zipWithIndex
	// We group by top-level URNs
	val v3:Vector[(CtsUrn, Vector[(CitableNode, Int)])] = {
		v2.groupBy( _._1.urn.collapsePassageTo(level) ).toVector
	}
	// GroupBy destroys order, but we have the original index for re-sorting
	val v4:Vector[(CtsUrn, Vector[(CitableNode, Int)])] = v3.sortBy(_._2.head._2)
	// Get rid of the stuff we don't need
	val v5:Vector[Vector[(CitableNode, Int)]] = v4.map(_._2)
	// Map to a Vector of Corpora
	val corpusVec:Vector[Corpus] = v5.map( v => {
		val nodes:Vector[CitableNode] = v.map(_._1)
		Corpus(nodes)
	})
	corpusVec
}


// Write out each of 24 books

	var htmlTop:String = s"""<!DOCTYPE html>
<html>
<head>
	<title>${groupName}: ${workTitle}</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<link rel="stylesheet" type="text/css" href="style.css">
</head>

<body>
"""

	var htmlBottom:String = """</body></html>"""

val bookChunks:Vector[Corpus] = chunkByCitation(popeCorpus, 1)

for ( bk <- bookChunks.zipWithIndex) {
	val bkNum:Int = bk._2 + 1
	val c:Corpus = bk._1

	// create a filename
	val htmlName:String = s"book${bkNum}.html"

	// navigation?
	val prevLink:String = {
		bkNum match {
			case n if (n == 0) => { "" }
			//case n if (n == (bookChunks.size - 1)) => { "" }
			case _ => { s"""<a href="book${bkNum - 1}.html">previous</a>""" }
		}
	}
	val nextLink:String = {
		bkNum match {
			case n if (n == (bookChunks.size - 1)) => { "" }
			case _ => { s"""<a href="book${bkNum + 1}.html">next</a>""" }
		}
	}
	val nav = s"""<div class="nav">${prevLink} | ${nextLink}</div>"""

	// create a container with all the CitableNodes for this chunk
	val containerOpen:String = """<div class="text">"""
	val containerClose:String = """</div>"""

	val passages:Vector[String] = c.nodes.map( n => {
		s"""<p><span class="cite">${n.urn.passageComponent}</span>${n.text}</p>"""
	})

	// save this chunk as an html file
	val htmlString:String = {
		htmlTop + nav + containerOpen + passages.mkString("\n") + containerClose + htmlBottom
	}
	// Write out to a file
	saveString(htmlString, "html/", htmlName)
}
