
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

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String):Vector[String] = {
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

/* Project-specific CEX Stuff */

val myCexFile:String = "pope_iliad.cex"

lazy val lib = loadLibrary(myCexFile)
lazy val tr = lib.textRepository.get
lazy val popeCorpus = tr.corpus

// Avoid typing lengthy URNs all the time
def u(passage:String):CtsUrn = {
	val baseUrl:String = "urn:cts:fufolio:pope.iliad.fu2019:"
	CtsUrn(s"${baseUrl}${passage}")
}

// Quick access to the ID of a poetic book
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
def chunk25(corp:Corpus):Vector[Corpus] = {
	// make Vectors with 25 passages in each
	val nodeChunks:Vector[Vector[CitableNode]] = corp.nodes.sliding(25,25).toVector
	// turn each of those into a Corpus
	val corpChunks:Vector[Corpus] = {
		nodeChunks.map( nodeVec => Corpus(nodeVec) )
	}
	// return that as a value
	corpChunks
}

// Write out 25 lines to a page

var htmlTop:String = s"""<!DOCTYPE html>
<html>
<head>
	<title>${groupName}: ${workTitle}</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<link rel="stylesheet" type="text/css" href="style.css">
</head>

<body>"""

var htmlBottom:String = """</body></html>"""

val bookChunks:Vector[Corpus] = chunk25(popeCorpus)

for ( bk <- bookChunks.zipWithIndex) {
	val bkNum:Int = bk._2 + 1
	val c:Corpus = bk._1

	// create a filename
	val htmlName:String = s"book${bkNum}.html"

	/* Navigation */
	val prevLink:String = {
		if (bkNum == 1){
			""
		} else {
			 s"""<a href="book${bkNum - 1}.html">previous</a>""" 
		}
	}
	val nextLink:String = {
		bkNum match {
			case n if (n == (bookChunks.size)) => { "" }
			case _ => { s"""<a href="book${bkNum + 1}.html">next</a>""" }
		}
	}

	// create a container with all the CitableNodes for this chunk
	val containerOpen:String = """<div class="text">"""
	val containerClose:String = """</div>"""

	val passages:Vector[String] = c.nodes.map( n => {
		s"""<p class="poetryLine"><span class="cite">${n.urn}</span>${n.text}</p>"""
	})

	// save this chunk as an html file
	val htmlString:String = {
		htmlTop + 
		containerOpen + 
		prevLink +
		passages.mkString("\n") + 
		prevLink +
		containerClose + 
		htmlBottom
	}
	// Write out to a file
	saveString(htmlString, "html/", htmlName)
}
