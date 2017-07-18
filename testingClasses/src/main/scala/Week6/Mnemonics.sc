import scala.io.Source


  /* read a file of words */
  val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

  /* create a list and filter all words where *all* their characters are not letters (like dashes) */
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

  val mnem = Map('2'-> "ABC", '3' -> "DEF",  '4' -> "GHI", '5' -> "JKL", '6'  -> "MNO", '7'  -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  //inver mnem map from num -> chars to chars -> num
  val charCode : Map[Char, Char] = {
    for((digit, str) <- mnem; ltr <- str) yield ltr -> digit
  }

  // map a word to the digit string it can represent
  def wordCode(word: String) : String = {
    word.toUpperCase  map charCode
  }

  //wordCode("java")

  //a map from digit strings to the words that represent them
  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue(Seq())

//encode a num as a set of strings that represented by the num
def encode (number: String) : Set[List[String]] = {
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode (number drop split)
    }yield word :: rest
  }.toSet
}

encode("7225247386")

def translate(number: String) : Set[String] = {
  encode(number) map (_ mkString " ")
}

  translate("7225247386")
