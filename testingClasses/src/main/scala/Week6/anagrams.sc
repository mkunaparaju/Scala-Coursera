import scala.io.Source


/* read a file of words */
val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

/* create a list and filter all words where *all* their characters are not letters (like dashes) */
val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

val dictionary: List[Word] = words

//val a =  words.head.toLowerCase groupBy(ltr => ltr) map { case(ch ,st) => (ch, st.length)} toList

 def wordOccurrences(w: Word) : Occurrences = {
  w.toLowerCase groupBy(ltr => ltr) map { case(ch ,st) => (ch, st.length)} toList
}

def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatten.mkString)

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  dictionary groupBy( w => wordOccurrences(w))
}

def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(List())
  case x :: xs => for {
    curr <- (0 to x._2).map((x._1, _)).toList
    comb <- combinations(xs)
  } yield if (curr._2 == 0) comb else curr :: comb
}

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  def iter(x: Occurrences, y: Occurrences, acc: Occurrences): Occurrences = (x, y) match {
    case (x, Nil) => acc ::: x
    case (Nil, y) => acc ::: y
    case (xx :: xs, yy :: ys) =>
      if (xx._1 < yy._1) iter(xs, y, acc :+ xx)
      else iter(xs, ys, acc :+ (xx._1, xx._2 - yy._2))
  }
  iter(x, y, List()).filter(_._2 != 0)
}