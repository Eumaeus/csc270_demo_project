
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





/* MD stuff */

var mdTop:String = s"""## ${groupName}

# ${workTitle}

"""

var mdBottom:String = "\n"

/* Now we build the website */

val bookChunks:Vector[Corpus] = chunkByCitation(popeCorpus, 1)


def buildSite:Unit = {
	for ( bk <- bookChunks.zipWithIndex) {

		// grab the chapter's id (in this case, the Book's number)
		val bkNum:Int = bk._2 + 1
		// grab the Corpus so it is easy to use
		val c:Corpus = bk._1

		// chunk again, by stanza
		val stanzaChunks:Vector[Corpus] = chunkByCitation(c, 2)

		// create a unique filename for each book
		val htmlName:String = s"book${bkNum}.md"


		/* Chapter Heading */
		val bookHeader:String = s"\n### Book ${bkNum}"

		// create a container with all the CitableNodes for this chunk

		// map stanzaChunks
		val stanzas:Vector[String] = {
			stanzaChunks.map( sc => {
				val open:String = "\n"
				val close: String = ""
				val passages:Vector[String] = sc.nodes.map( n => {
					val fixedText:String = {
						n.text
							.replaceAll("'","’")
							.replaceAll("""^"""","“")
							.replaceAll("""" ""","”")
							.replaceAll(""""$""","”")
					}
					s"""**${n.urn.passageComponent}**\t${fixedText}"""
				})
				Vector(open) ++ passages ++ Vector(close)
			}).flatten
		}

		

		// save this chunk as an html file
		val mdString:String = {
			mdTop + 
			bookHeader + 
			stanzas.mkString("\n") + 
			mdBottom
		}
		// Write out to a file
		saveString(mdString, "md/", htmlName)
	}
}

println(s"\n-------------\nUse 'buildSite' to make Markdown pages\n-------------\n")
