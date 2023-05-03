package forcomp

import scala.io.{ Codec, Source }

object Anagrams extends AnagramsInterface:

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
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
 // val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy((chr:Char) => chr).map( x => (x._1,x._2.length)).toList.sorted


  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString("").filterNot(_.isWhitespace))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    Dictionary.loadDictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match
    case Some( head :: tail ) => head :: tail
    case Some(Nil) => List()
    case None => List()

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
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
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match
      case Nil => List(Nil) //List[Occurrences]()
      case (c, n) :: Nil => getsubs((c,n)).map(item => List(item))
      case (c, n) :: tail => 
        (for i <- getsubs((c,n))
            j <- combinations(tail)
        yield i :: j).map(item => item.filter((c,n) => n > 0))

  def getsubs(item: (Char,Int)): Occurrences = 
    (for i <- (0 to item._2) yield (item._1,i)).toList
  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = 
    x.filter( item => ! y.contains(item)).map( item => item match
      case (c: Char,n: Int) =>  if y.exists( item => item._1 == c) then (c,n-y.toMap.get(c).get) else (c,n)
      ).toList

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
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
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   * 
   * 
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    val sOccurrences = sentenceOccurrences(sentence)
    doPossibles(combosList(sOccurrences).filter( x => pruneValid(x,sOccurrences))) 
    
  def doPossibles(combosList: List[List[Occurrences]]): List[Sentence] = combosList match
    case List() => List()
    case head :: tail => (getSentences(head) ::: doPossibles(tail))

  def getSentences(slist: List[Occurrences]): List[Sentence] = slist match
    case List() => List(Nil)
    case head :: Nil =>dictionaryByOccurrences(head).map(x => List(x))
    case head :: tail => 
      for word <- dictionaryByOccurrences(head)
          rest <- getSentences(tail)
      yield word :: rest

  def pruneValid(item: List[Occurrences], sOccurences: Occurrences): Boolean =
    item match
      case List() => if sOccurences == List() then true else false
      case head :: tail => pruneValid(tail,subtract(sOccurences,head))

  def combosList(sOccurrences: Occurrences): List[List[Occurrences]] =
    val combos = combinations(sOccurrences).filter(x => dictionaryByOccurrences.contains(x))
  
    combos match
      case List() => List[List[Occurrences]](Nil)
      case _ => 
        for item <- combos
          lst <- combosList(subtract(sOccurrences,item))
        yield item :: lst

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
