
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

val demoLib:String = "jrrt.cex"


// A utility for printing out Vectors and other values to the screen
def showMe(v:Any):Unit = {
	v match {
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

// Load a CITE Library
def loadLibrary(fp:String = demoLib):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

// Load any old file to a Vector of Strings
def loadFile(fp:String):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

// Save a String to a file
def saveString(s:String, filePath:String = "csv/", fileName:String = "readingScore.tsv"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}

// For dividing strings into word-tokens
val splitters:String = """[\[\])(:·⸁.,·;';   "?·!–—⸂⸃]"""

// For dividing strings into sentences
val sentencePunc:String = """[.;?!]"""

// A dumb stab at syllabifying English
def countSyllables(s:String):(String, Int) = {
	val dumbSyllabifier = """[^aeiou]*[aeiouy]+""".r
	val initialCount = dumbSyllabifier.findAllIn(s).size
	val addYRegex = """[^aeiou]y[^aiou]""".r
	val addIousRegex = """ious""".r
	val addYs:Int = addYRegex.findAllIn(s).size
	val addIous:Int = addIousRegex.findAllIn(s).size
	val modString:String = {
		if (initialCount == 1) {
			s
		} else {
			s.replaceAll("es?$","")
		}
	 }
	val syllables:Int = dumbSyllabifier.findAllIn(modString).size + addYs + addIous
	(s, syllables)
}

// Given a string, count the words and return that as a Double
def totalWords(s:String):Double = {
	val words:Int = s.split(splitters).toVector.filter(_.size > 0).map(_.toLowerCase).size
	words.toDouble
}

// Given a string, count the syllables and return that as a Double
def totalSyllables(s:String):Long = {
	 val words:Vector[String] = s.split(splitters).toVector.filter(_.size > 0).map(_.toLowerCase)
	 val syllVec:Vector[(String,Int)] = words.map( w => {
			countSyllables(w)
		})
	 val longVec:Vector[Long] = syllVec.map(_._2.toLong)
	 longVec.sum
}

// Given a string, count its sentences
def totalSentences(s:String):Long = {
	s.split(sentencePunc).toVector.map(_.size > 0).size
}

// Using the above methods, return the Flesch-Kinkaid Grade Level
def getReadingScore(s:String):Double = {
		val words:Double = totalWords(s)
		val sentences:Double = totalSentences(s)
		val syllables:Double = totalSyllables(s)
		val element1:Double = {
			0.39 * (words / sentences)
		}
		val element2:Double = {
			11.8 * ( syllables / words )
		}
		val element3:Double = 15.59
		val readingEase:Double = element1 + element2 - element3
		readingEase
}


// Actually load a library and get a corpus!
val tr = loadLibrary().textRepository.get
val corp = tr.corpus

// Options for how to chunk the text for analysis
val chunkSize:Int = 200
val stepSize:Int = 1 

// Divide the text into citable chunks
val chunks:Vector[(CtsUrn,String)] = {
	val slidChunks:Vector[Vector[CitableNode]] = corp.nodes.sliding(chunkSize,stepSize).toVector
	val mappedChunks:Vector[(CtsUrn,String)] = {
		slidChunks.map( sc => {
			val startUrn:CtsUrn = sc.head.urn
			val endUrnPassage:String = sc.last.urn.passageComponent
			val newUrn:CtsUrn = CtsUrn(s"${startUrn}-${endUrnPassage}")
			val text:String = sc.map( t => t.text).mkString(" ")
			(newUrn, text)
		})
	}
	mappedChunks
}

// Map those to Citation + Grade Level
val fkChunks:Vector[(CtsUrn,Double)] = {
	chunks.map(c => {
		(c._1, getReadingScore(c._2))
	})
}

// Make a string, with the passages in text-order
val fkStringUnsorted:String = {
	fkChunks.map( fkc => {
		s"${fkc._1}\t${fkc._2}"	
	}).mkString("\n")
}

// Make a second, with the passages in ascending order of difficulty
val fkStringSorted:String = {
	fkChunks.sortBy(_._2).map( fkc => {
		s"${fkc._1}\t${fkc._2}"	
	}).mkString("\n")
}

// Go ahead and report the hardest and easiest chunks!
val hardestChunks:Unit = {
	val sortedVec:Vector[String] = fkChunks.sortBy(_._2).map(c => {
		s"${c._1}"
	})
	val easiestFive:Vector[String] = sortedVec.take(5)
	println(s"\nEasiest 5:\n-----------")
	showMe(easiestFive)
	val hardestFive:Vector[String] = sortedVec.reverse.take(5)
	println(s"\nHardest 5:\n-----------")
	showMe(hardestFive)
}


saveString(fkStringUnsorted, "csv/", "unsorted.tsv")
saveString(fkStringSorted, "csv/", "sorted.tsv")


