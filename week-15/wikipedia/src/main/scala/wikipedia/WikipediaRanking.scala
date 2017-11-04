package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import org.apache.spark.rdd.RDD
import org.apache.spark.rdd.PairRDDFunctions

/**
 * WikipediaArticle contains the title and the text of the article.
 */
case class WikipediaArticle(title: String, text: String) {

  type Language = String
  type LanguageArticlePair = (String, WikipediaArticle)

  /**
   * @return whether the text of this article mentions `lang` or not
   * @param lang language to look for (e.g. "Scala")
   */
  def mentionsLanguage(lang: Language): Boolean =
    text.split(' ').contains(lang)

  def mentioningPair(languages: Seq[Language]) : Seq[LanguageArticlePair] =
    languages.filter(mentionsLanguage).map((language:String) => (language,this))

}

object WikipediaRanking {

  type Language = String
  type LanguageArticlePair = (String, WikipediaArticle)

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
  def occurrencesOfLang(lang: Language, rdd: RDD[WikipediaArticle]): Int = {
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
  def rankLangs(langs: List[Language],
                rdd  : RDD[WikipediaArticle]): List[(Language, Int)] = {

    val langWithCounts: List[(Language, Int)] =
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
   *   following type: 'RDD[ (Language, Iterable[WikipediaArticle]) ]'
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
  def makeIndex(languages: List[Language],
                articles: RDD[WikipediaArticle]): RDD[(Language, Iterable[WikipediaArticle])] =  {
    /**
     * Map article to (article,langauge) pair. Create (article, langauge)
     * pairs for all articles.
     */
    val articleLanguage: RDD[LanguageArticlePair] =
      articles.flatMap(_.mentioningPair(languages)).persist()

    new PairRDDFunctions(articleLanguage).groupByKey()
  }

  /**
   * (2) Compute the language ranking again, but now using the inverted index.
   *     Can you notice a performance improvement?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangsUsingIndex(index: RDD [(String, Iterable[WikipediaArticle])]): List[(String ,Int)] = {
    val langCount =
      index.map({
        case (language:Language, articles:Iterable[WikipediaArticle]) => (language, articles.size)
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
  def rankLangsReduceByKey(languages: List[String],
                           articles: RDD[WikipediaArticle]): List[(String, Int)] = {

    // Pair RDD article with language it contains.
    val articleLanguage: RDD[LanguageArticlePair] =
      articles.flatMap(_.mentioningPair(languages)).persist()

    // First map all languages to single element lists.
    val retval = new PairRDDFunctions(articleLanguage).countByKey().toList.map(element => (element._1, element._2.toInt))

    retval.sortBy(_._2).reverse
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
