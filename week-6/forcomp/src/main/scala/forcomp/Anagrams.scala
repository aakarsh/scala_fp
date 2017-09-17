package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrence = (Char, Int)

  type Occurrences = List[(Char, Int)]

  def Occurrences(xs:(Char,Int)*) = List[(Char,Int)](xs:_*)

  /**
   * The dictionary is simply a sequence of words.  It is predefined
   * and obtained as a sequence using the utility method
   * `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary 

  /**
   * Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are
   * treated as the same character, and are represented as a lowercase
   * character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   *
   */
  def wordOccurrences(w: Word): Occurrences = {
    val groupedChars   = w.toLowerCase.sorted.groupBy(_.toChar)
    val freqMap = groupedChars.map({ case (char:Char, repetitions:String) => (char , repetitions.length)} )
    freqMap.toList.sortBy({case (char:Char,freq:Int) => char})
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val wo  = s.map(wordOccurrences)
    val empty = List[Occurrence]()
    wo.foldLeft(empty){(z:Occurrences,f:Occurrences) => addOccurences(z,f)}

  }

  /**
   *  The `dictionaryByOccurrences` is a `Map` from different
   *  occurrences to a sequence of all the words that have that
   *  occurrence count.  This map serves as an easy way to obtain all
   *  the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character
   *  occurrence list:
   *
   *  `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain
   *  an entry:
   *
   *  List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {

    /**
     * Adds a word to given map by word occurence, if the word exists
     *  then append the word to the already occuring.
     */
    def addWord(res : Map[Occurrences, List[Word]], word: Word) : Map[Occurrences, List[Word]] = {
      val occ = wordOccurrences(word)
      res.get(occ) match {
        case Some(words:List[Word]) => (res - occ) + (occ -> (word::words))
        case None => res + (occ -> List(word))
      }
    }

    val emptyMap = Map[Occurrences, List[Word]]()
    dictionary.foldLeft(emptyMap)(addWord)
  }

  /**
   * Returns all the anagrams of a given word.
   */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Returns the list of all subsets of the occurrence list.
   *
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   *
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))`
   * are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2)))
   *
   *  Note that the order of the occurrence list subsets does not
   *  matter -- the subsets in the example above could have been
   *  displayed in some other order.
   *
   * Takes a list of occurences and generates all subsets
   * of the given list of occurences.
   *
   * type Occurrences = List[(Char, Int)]
   *
   */


  def combinations(occurrences: List[Occurrence]): List[Occurrences] = {

    // Skip Empty Frequency - an empty frequency is used to combine
    def skip(occur: Occurrence): Boolean =
      occur match {case (_,frequency) => (frequency <= 0) }

    // Convertes occurence to a skipping empty
    def wrap(occur :Occurrence): List[Occurrence] = List(occur)

    // For an given occurance generate list of options
    // all possible values of the frequency from (a,0)...(a,freq)
    def generate(occur: Occurrence): List[Occurrence] =
      occur match {
        case (letter:Char,frequency:Int) =>
          val options =  for ( i <- 0 to frequency) yield (letter,i)
          options.toList
      }

    val empty = List[Occurrences](Nil) // Empty options

    occurrences match {
      case Nil   => empty  // cs - is list of occurences
      case headOccurrence :: restOccurrences => {
        // Generate head options using first occureance
        // Perform cartisian product with combinations generated by rest occurences
        product_preppend(generate(headOccurrence),combinations(restOccurrences),wrap ,skip).toList
      }
    }
  }

  /**
   *
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {

    val y_freq = y.toMap

    def positive_frequency (occ:Occurrence) : Boolean =
      occ match { case (_,frequency) => frequency > 0  }

    def letter(occ:Occurrence): Char =
      occ match { case (letter,_) => letter }

    def sub(occ:Occurrence): Occurrence =
      occ match { case (letter,x_freq) => (letter, x_freq - y_freq.getOrElse(letter,0)) }

    x.toMap.map(sub).toList.sortBy(letter).filter(positive_frequency)
  }


  def addOccurences(x: Occurrences, y: Occurrences): Occurrences = {

    val y_freq = y.toMap

    def positive_frequency (occ:Occurrence) : Boolean =
      occ match { case (_,frequency) => frequency > 0  }

    def letter(occ:Occurrence): Char =
      occ match { case (letter,_) => letter }

    def addY(occ:Occurrence): Occurrence =
      occ match { case (letter,x_freq) => (letter, x_freq + y_freq.getOrElse(letter,0)) }

    val xy = x.toMap.map(addY)
    val only_y = y_freq.filter({case (l,v) => !xy.contains(l)})
    ( xy ++ only_y).toList.sortBy(letter).filter(positive_frequency)
    
  }

  /**
   *  Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of
   *  all the characters of all the words in the sentence, and
   *  producing all possible combinations of words with those
   *  characters, such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not
   *  have to correspond.  For example, the sentence `List("I",
   *  "love", "you")` is an anagram of the sentence `List("You",
   *  "olive")`.
   *
   *  Also, two sentences with the same words but in a different ordero
   *  are considered two different anagrams.  For example, sentences
   *  `List("You", "olive")` and `List("olive", "you")` are different
   *  anagrams of `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and
   *  its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order
   *  shown above - any order is fine as long as all the anagrams are
   *  there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the
   *  dictionary, then the sentence is the anagram of itself, so it
   *  has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   *
   */

  /**
   * Construct a product list where ever head from heads is appended to every tail
   * in tails. The result is a cartesian product of head list and tail list.
   */
  def product_preppend[T](heads:List[T], tails:List[List[T]], unit:T =>List[T],skip:T => Boolean ): List[List[T]] = {
    tails match {
      case Nil => heads.map( (head:T) => { if (skip(head)) List() else unit(head) }) /* list of list of heads */
      case tails =>  /** list with every head appended to ever tail */
        for{ head <- heads
             tail <- tails}
        yield head match {
          case head if skip(head) => tail
          case head               => head::tail
        }
    }
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    /**
     * Takes the sentence and generate a list of all combinations of
     * the sentences.
     *
     * Each individual combination is a occurance list which can be
     * looked to seach the dictionary.
     */
    val letterOccurences = sentenceOccurrences(sentence)

    def frequency (occ:Occurrence) : Int = occ match { case (_,frequency) => frequency  }
    def total_size(occList: List[Occurrence]): Int = occList.map(frequency).sum
    def sentence_length(words : List[Word]): Int = words.map(_.length).sum
    def hasAnagrams (occ: Occurrences) : Boolean = dictionaryByOccurrences.contains(occ)

    def anagrams(occList : Occurrences) : List[Sentence]= {

      occList match {

        case Nil => List[Sentence](List[Word]())

        case occList => {

          val occHeads:List[Occurrences] = combinations(occList).filter(hasAnagrams)

          /**
           * Tail list is the remaing part for a combination of occHead list.
           */
          val occTails:List[Occurrences] = occHeads.map(occHead => subtract(occList,occHead))

          /**
           * For every possible occurance combination which has an
           * anagram. We collect a list of these anagarams. For example.
           * [[(a,1)(e,1),(p,1)],...] -> [[(ape),..],...]
           * Thus each occuerence list gets transformed to a word list of anagrams.
           */
          val headAnagrams : List[List[Word]] = occHeads.map(dictionaryByOccurrences)

          /**
           * Each tail list gets mapped to a corresponding set of
           * anagram sentences corresponding to the particular tail
           * list.
           */

          val tailAnagrams : List[List[Sentence]] = occTails.map(anagrams)

          /**
          * Thus for each combination we have list of head_anagrams
          * and a sentences of tail anagrams.
          * 
          * zip the head anagrams with their coressponding tail anagrams
          */
          val headWithTail = headAnagrams.zip(tailAnagrams)

          // Once we have producted to produce sentences, flattened
          // we need to consider flatMaps

          def skip(word:Word) :Boolean   = word.isEmpty()
          def wrap(word:Word):List[Word] = List[Word](word)

          val headTailProduct:List[Sentence] = headWithTail.flatMap(
            {case (head: List[Word], tail: List[Sentence] ) => product_preppend(head,tail,wrap, skip) })

          var s:List[Sentence]  = (headTailProduct.toSet).toList

          val length = total_size(occList)

          //product
          //s = s.filter((s:Sentence)  => sentence_length(s) == length)
          //println("length: "+length+" occ "+occList+" s:"+s)

          s
        }
      }
    }


    var sentences2 : List[Sentence] = anagrams(letterOccurences)

    println(sentences2)

    sentences2 = sentences2.filter((s:Sentence) => sentence_length(s) == total_size(letterOccurences))

    sentences2 = sentences2.sortBy(x => x.toString)

    sentences2.foreach(println)

    //println("sentences:" + sentences2)
    // Need a list of sentences but getting a list of words
    sentences2

  }

}
