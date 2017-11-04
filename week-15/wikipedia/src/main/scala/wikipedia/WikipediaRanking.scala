package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import org.apache.spark.rdd.RDD

/**
 * WikipediaArticle contains the title and the text of the article.
 */
case class WikipediaArticle(title: String, text: String) {

  /**
   * @return whether the text of this article mentions `lang` or not
   * @param lang language to look for (e.g. "Scala")
   */
  def mentionsLanguage(lang: String): Boolean =
    text.split(' ').contains(lang)

}

object WikipediaRanking {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  val numCPUInfo = 4

  /**
   * Local spark configuration goes here.
   */
  val conf: SparkConf =
    new SparkConf()
      .setMaster("local")
      .setAppName("wikipedia")

  /**
   * Spark context used to create RDDs.
   */
  val sc: SparkContext = new SparkContext(conf)

  /**
   * Use spark context to read the text file and parse its lines
   * into a RDD sequence of WikipediaArticle.
   */
  val wikiRdd : RDD[WikipediaArticle] = {
    val lines : RDD[String] = sc.textFile(WikipediaData.filePath)
    lines.map(WikipediaData.parse)
  }

  /**
   *  Returns the number of articles in which the language `lang` occurs.
   *
   *  Hint1: Consider using method `aggregate` on RDD[T].
   *  Hint2: Consider using method `mentionsLanguage` on `WikipediaArticle`.
   */
  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int = {
    rdd.persist()

    def fold(acc:Int , article: WikipediaArticle): Int =
      if( article.mentionsLanguage(lang) ) (acc + 1) else acc

    def combine(count1: Int, count2: Int): Int = count1 + count2

    val zeroValue: Int = 0

    rdd.aggregate(zeroValue) (fold, combine)

  }

  /**
   * (1)
   * Use `occurrencesOfLang` to compute the ranking of the languages
   * (`val langs`) by determining the number of Wikipedia articles
   * that mention each language at least once. Don't forget to sort the
   * languages by their occurrence, in decreasing order!
   *
   * Note: this operation is long-running. It can potentially run for
   * several seconds.
   */
  def rankLangs(langs: List[String],
                rdd  : RDD[WikipediaArticle]): List[(String, Int)] = {

    val langWithCounts: List[(String, Int)] =
      langs.map(lang => (lang, occurrencesOfLang(lang,rdd)))

    // Sort by frequency of occurence in descending order.
    langWithCounts.sortBy(_._2).reverse
  }

  /**
   * Compute an Inverted-Index:
   *
   * An inverted index is an index data structure storing a mapping
   * from content, such as words or numbers, to a set of documents.
   *
   * In particular, the purpose of an inverted index is to allow fast
   * full text searches. In our use-case, an inverted index would be
   * useful for mapping from the names of programming languages to the
   * collection of Wikipedia articles that mention the name
   * at least once:
   *
   * - To make working with the dataset more efficient and more convenient,
   *   implement a method that computes an 'inverted index' which maps
   *   programming language names to the Wikipedia articles on which
   *   they occur at least once.
   *
   * - Implement method 'makeIndex' which returns an 'RDD' of the
   *   following type: 'RDD[ (String, Iterable[WikipediaArticle]) ]'
   *
   *   This RDD contains pairs, such that:
   *
   * - For each language in the given language list there is at most one
   *   pair.
   *
   * - Furthermore, the second component of each pair (the Iterable)
   *   contains the WikipediaArticle that mention the language
   *   at least once.
   */
  def makeIndex(langList: List[String],
                articles: RDD[WikipediaArticle]) : RDD[ (String, Iterable[WikipediaArticle]) ] =  {

    // Take the list of languages. Take list of articles
    // For each language map the article to an language
    //
    val languages =  langList.toVector

    //
    // TODO need ot handle cases without the language mentioned.
    //
    // var empty = languages.map( (_, List[WikipediaArticle]()) )

    val languageIndexMap = (languages, (0 until languages.size)).zipped.toMap

    /**
     * Map article to (article,langauge) pair.
     */
    def makeArticleLanguagePairs( article : WikipediaArticle ):  List[(WikipediaArticle, String)] = {
      val retval =
        for( language <- languages if article.mentionsLanguage(language))
          yield (article, language)
      retval.toList
    }

    // Create (article, langauge) : pairs for all articles.
    val articleLanguage : RDD[( WikipediaArticle, String )] =
      articles.flatMap(makeArticleLanguagePairs).persist()

    def article2LanguagePartition(pair: (WikipediaArticle, String)) : Int =
      pair match {
        case (article: WikipediaArticle, language: String) =>
          languageIndexMap(language)
      }

    val languageGroupings: RDD[(Int, Iterable[(WikipediaArticle, String)])] =
      articleLanguage.groupBy( article2LanguagePartition(_) , languages.size )

    def toLanguageArticleMap(retval: RDD[(Int, Iterable[(WikipediaArticle, String)])]) = {

      def toArticles(articleLanguagePairs :Iterable[(WikipediaArticle, String)]) = {
        articleLanguagePairs.map({ case (article,language) => article })
      }

      retval.map({
        case (languageIndex:Int, articleLanguagePairs: Iterable[(WikipediaArticle,String)]) =>
          (languages(languageIndex), toArticles(articleLanguagePairs))
      })
    }

    // Convert to [ lang_1 => (articles, ... ) ,lang_2 => (articles, ... )
    toLanguageArticleMap(languageGroupings)

  }

  /**
   * (2) Compute the language ranking again, but now using the inverted index.
   *     Can you notice a performance improvement?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangsUsingIndex(index: RDD [(String, Iterable[WikipediaArticle])]): List[(String ,Int)] = {

    index.persist()

    val langCount =
      index.map({
        case (language:String, articles:Iterable[WikipediaArticle]) => (language, articles.size)
      })

    val ranking  = langCount.sortBy(_._2)

    ranking.collect().reverse.toList
  }

  /**
   * Combine index and ranking creation.
   *
   * (3) Use `reduceByKey` so that the computation of the index and
   *     the ranking are combined.  Can you notice an improvement in
   *     performance compared to measuring *both* the computation of
   *     the index and the computation of the ranking? If so, can you
   *     think of a reason?
   *
   *  Note: this operation is long-running. It can potentially run for
   *  several seconds.
   */
  def rankLangsReduceByKey(langList: List[String], 
                           wikiRdd: RDD[WikipediaArticle]): List[(String, Int)] = {
    
    val langauges = langList.toVector
    val empty
    
    /**
     * Rank languages attempt #3: rankLangsReduceByKey
     *
     * In the case where the inverted index from above is only used
     * for computing the ranking and for no other task (full-text
     * search, say), it is more efficient to use the reduceByKey
     * method to compute the ranking directly, without first-computing
     * an inverted index.
     *
     * Note that the reduceByKey method is only defined for RDDs
     * containing pairs (each pair is interpreted as a key-value
     * pair).
     *
     * Implement the rankLangsReduceByKey method,
     * this time computing the ranking without the inverted index,
     * using reduceByKey.
     *
     * Like in part 1 and 2, rankLangsReduceByKey should compute a
     * list of pairs where the second component of the pair is the
     * number of articles that mention the language (the first
     * component of the pair is the name of the language).
     * 
     * Again, the list should be sorted in descending order.
     *
     * That is, according to this ranking, the pair with the highest
     * second component (the count) should be the first element of the
     * list.
     * 
     * Can you notice an improvement in performance compared to
     * measuring both the computation of the index and the computation
     * of the ranking as we did in attempt #2?
     *
     * If so, can you think of a reason?

     */
    /**
     * def index: RDD[(String, Iterable[WikipediaArticle])] =
     * makeIndex(langs, wikiRdd)
     *
     * //  Languages ranked according to (2), using the inverted index.
     * val langsRanked2: List[(String, Int)] =
     *     timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))
     */

    ???
  }

  def main(args: Array[String]) {

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] =
      timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] =
      makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] =
      timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] =
      timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

    /* Output the speed of each ranking */
    println(timing)
    sc.stop()

  }

  val timing = new StringBuffer

  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }

}
