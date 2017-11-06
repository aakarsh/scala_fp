package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import annotation.tailrec
import scala.reflect.ClassTag

import org.apache.spark.rdd.PairRDDFunctions

/**
 * A raw stackoverflow posting, either a question or an answer.
 */
case class Posting(postingType: Int,
                   id: Int,
                   acceptedAnswer: Option[Int],
                   parentId: Option[QID],
                   score: Int,
                   tags: Option[String]) extends Serializable {

  def isQuestion = postingType == 1
  def isAnswer   = postingType == 2


}

/**
 * The main class.
 */
object StackOverflow extends StackOverflow {

  @transient
  lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")

  @transient
  lazy val sc: SparkContext = new SparkContext(conf)

  /** Main function */
  def main(args: Array[String]): Unit = {

    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")

    val raw     = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored  = scoredPostings(grouped)

    /**
     * Vector postings is the mapping from (language -> highest rated answer)
     * for all questions on the site.
     */
    val vectors = vectorPostings(scored)

    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())

    // Find k-means using randomly sampled subset of questions
    val means   = kmeans(sampleVectors(vectors), vectors, debug = true)

    val results = clusterResults(means, vectors)

    printResults(results)

  }
}


/** The parsing and kmeans methods */
class StackOverflow extends Serializable {

  /**
   * Languages: List of the popular programming languages.
   */
  val langs =
    List("JavaScript", "Java", "PHP",  "Python", "C#", "C++", "Ruby", "CSS",
         "Objective-C","Perl", "Scala","Haskell","MATLAB", "Clojure", "Groovy")

  /**
   * K-means parameter: how "far apart" languages should be for the kmeans algorithm?
   */
  def langSpread = 50000

  assert(langSpread > 0, "If langSpread is zero we can't recover the language from the input data!")

  /**
   * K-means parameter: Number of clusters
   */
  def kmeansKernels = 45

  /**
   * K-means Parameter: Convergence Criteria
   */
  def kmeansEta: Double = 20.0D

  /**
   * K-means Parameter: Maximum iterations
   */
  def kmeansMaxIterations = 120

  /**
   * Parsing utilities:
   */
  /** Load postings from the given file */
  def rawPostings(lines: RDD[String]): RDD[Posting] =
    lines.map(line => {

      val arr = line.split(",")

      Posting(postingType =    arr(0).toInt,
              id =             arr(1).toInt,
              acceptedAnswer = if (arr(2) == "") None else Some(arr(2).toInt),
              parentId =       if (arr(3) == "") None else Some(arr(3).toInt),
              score =          arr(4).toInt,
              tags =           if (arr.length >= 6) Some(arr(5).intern()) else None)
    })


  /**
   *
   * Group the questions and answers together
   */
  def groupedPostings(postings: RDD[Posting]): RDD[(QID, Iterable[(Question, Answer)])]   = {

    /**
     * Then prepare them for a join operation by:
     *
     * 1. Extracting the QID value in the first
     * element of a tuple.
     */
    val questions = postings.filter(_.isQuestion).map( p => (p.id, p))
    val answers   = postings.filter(_.isAnswer)  .map( p => (p.id, p))

    /**
     * 2. Use one of the join operations (which one?)
     *    to obtain an RDD[(QID, (Question, Answer))]
     *
     *    Drop quesitons without answers.
     */
    val qa: RDD[(Int, (Question, Answer))] =
      questions.join(answers)

    /**
     * 3. Obtain an RDD[(QID, Iterable[(Question, Answer)])]
     */
    new PairRDDFunctions(qa).groupByKey()

  }

  /**
   * Compute the maximum score for each posting.
   */
  def scoredPostings(grouped: RDD[(QID, Iterable[(Question, Answer)])]):
              RDD[(Question, HighScore)] = {

    type QA = (Question, Answer)
    type QH = (Question, HighScore)

    def maxScore(q1: QA,  q2: QA): QA = {
      val (a1,a2) = (q1._2, q2._2) // answers
      if (a1.score > a2.score) q1 else q2
    }

    def toQuestionScore(qa: QA): QH = (qa._1, qa._2.score)

    val questionScore =
          grouped.mapValues(_.reduce(maxScore))
                 .mapValues(toQuestionScore)
                 .values

    questionScore
  }

  /**
   * Compute the vectors for the kmeans. The vector contains
   * the (languageindex * languageSpread) along with
   * as well as the high score.
   */
  def vectorPostings(scored: RDD[(Question, HighScore)]): RDD[(LangIndex, HighScore)] = {
    type QH = (Question, HighScore)

    /**
     * Return optional index of first language that occurs in `tags`.
     */
    def firstLangInTag(tag: Option[String],
                       languages: List[String]): Option[Int] = {
      if (tag.isEmpty) None
      else if (languages.isEmpty) None
      else if (tag.get == languages.head) Some(0) // Index: 0
      else {
        val tmp = firstLangInTag(tag, languages.tail)
        tmp match {
          case None => None
          case Some(i) => Some(i+1) // index i in languages.tail => index i+1
        }
      }
    }

    scored.map(qh =>
      firstLangInTag(qh._1.tags,langs) match {
        case Some(languageIndex) => ((languageIndex * langSpread), qh._2)
        case None => (-1, qh._2)
      }).filter(_._1 != -1)
  }

  /**
   * sample (Langauge, HighestScore) of points for all questions
   */
  def sampleVectors(points: RDD[(LangIndex, HighScore)]): Array [(Int, Int)] = {

    // kmeansKernels :- Number of clusters we are searching for
    assert(kmeansKernels % langs.length == 0,
           "kmeansKernels should be a multiple of the number of languages studied.")
    /**
     * Reservoir_sampling is used when we want to sample `size` number of
     * elements from an iterator without knowing in advance how many items
     * will be sampled.
     *
     * http://en.wikipedia.org/wiki/Reservoir_sampling
     */
    def reservoirSampling(lang: Int, iter: Iterator[Int], size: Int): Array[Int] = {

      val res = new Array[Int](size)
      val rnd = new util.Random(lang)

      for (i <- 0 until size) {
        assert(iter.hasNext, s"iterator must have at least $size elements")
        res(i) = iter.next
      }

      // Number of elements seen.
      var i = size.toLong

      // conditional replacement
      while (iter.hasNext) {
        val e = iter.next
        val j = math.abs(rnd.nextLong) % i
        if (j < size)
          res(j.toInt) = e
        i += 1
      }
      res
    }

    val res =
      if ( langSpread < 500 )
        // sample the space regardless of the language
        points.takeSample(false, kmeansKernels , 42)

      else {

        // sample the space uniformly from each language partition
        // key:  language-index.

        val perLang = kmeansKernels / langs.length

        points.groupByKey.flatMap({
          case (language, points) => {

            // points grouped by the language-index
            val languageSampledPoints =
              reservoirSampling(language, points.toIterator, perLang)

            languageSampledPoints.map(point => (language, point))
          }
        }).collect()
      }

    assert(res.length == kmeansKernels, res.length)

    res
  }
  /**
   *  Kmeans method: main kmeans computation
   */
  @tailrec
  final def kmeans(means:  Array[(Int, Int)],
                   points: RDD[(Int, Int)], iter:   Int = 1,
                   debug: Boolean = false): Array[(Int, Int)] = {

   /**
    * 1. Pick k-points called means. this is called initialization.
    */

    /**
     * 2. Associate each input point with the mean that is closest to it.
     *    we obtain k-clusters of points, and we refer to this process as
     *    classifying the points.
     */
    // Returns a new set of means based on old means ....

        
    val meanWithPoint = points.map(point => (findClosest(point,means),point))

    // Group by Mean
    val grouped = new PairRDDFunctions(meanWithPoint).groupByKey()

    // New Means
    val groupedByNewMeans:RDD[(Int,(Int,Int))] = grouped.mapValues(averageVectors)

    // TODO: This needs to do the group assignment ...
    //val newMeans = means.clone() // You need to compute newMeans.

    val newMeans = groupedByNewMeans.sortBy(_._1).values.collect()

    // The distance bwetween new means nad old means
    val distance : Double = euclideanDistance(means, newMeans)

    /**
     * 3. Update each mean to have the average value
     *    of the corresponding cluster.
     *
     *    If the K-means have significantly changed:
     *
     *    3.1. Go back to step 2.
     *
     *    3.2. If they did not, we say that,
     *         the algorithm converged.
     *
     * The K-means represent different clusters -- every point is
     * in the cluster corresponding to the closest-mean.
     *
     */
    if (debug) {
      println(s"""Iteration: $iter
                 |  * current distance: $distance
                 |  * desired distance: $kmeansEta
                 |  * means:""".stripMargin)
      for (idx <- 0 until kmeansKernels)
      println(f"   ${means(idx).toString}%20s ==> ${newMeans(idx).toString}%20s  " +
              f"  distance: ${euclideanDistance(means(idx), newMeans(idx))}%8.0f")
    }


    if (converged(distance))  // [new and old means] have sufficiently converged stop
      newMeans
    else if (iter < kmeansMaxIterations) // keep iterating
      kmeans(newMeans, points, iter + 1, debug)
    else {
      if (debug) {
        println("Reached max iterations!")
      }
      newMeans // Can not converge further.
    }
  }


  /**
   * Kmeans utilities: Decide whether the kmeans clustering converged.
   */
  def converged(distance: Double) = distance < kmeansEta

  /**
   * Return the euclidean distance between two points.
   */
  def euclideanDistance(v1: (Int, Int),
                        v2: (Int, Int)): Double = {

    val part1 = (v1._1 - v2._1).toDouble * (v1._1 - v2._1)
    val part2 = (v1._2 - v2._2).toDouble * (v1._2 - v2._2)
    part1 + part2
  }

  /**
   * return the euclidean distance between two points.
   */
  def euclideanDistance(p1: Array[(Int, Int)],
                        p2: Array[(Int, Int)]) : Double = {

    assert(p1.length == p2.length)

    var sum = 0d
    var idx = 0

    while(idx < p1.length) {
      sum += euclideanDistance(p1(idx), p2(idx))
      idx += 1
    }
    sum
  }

  /**
   * Return the closest point.
   */
  def findClosest(p: (Int, Int), centers: Array[(Int, Int)]): Int = {
    var bestIndex = 0
    var closest = Double.PositiveInfinity
    for (i <- 0 until centers.length) {
      val tempDist = euclideanDistance(p, centers(i))
      if (tempDist < closest) {
        closest = tempDist
        bestIndex = i
      }
    }
    bestIndex
  }


  /** average the vectors */
 def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int) = {

    val iter = ps.iterator

    var count = 0
    var comp1: Long = 0
    var comp2: Long = 0

    while( iter.hasNext ) {
      val item = iter.next
      comp1 += item._1
      comp2 += item._2
      count += 1
    }

    ((comp1 / count).toInt, (comp2 / count).toInt)
  }

  /**
   * Displaying results:
   */
  def clusterResults(means: Array[(Int, Int)], 
                     vectors: RDD[(LangIndex, HighScore)]): Array[(String, Double, Int, Int)] = {

    type Point = (Int,Int)

    val closest:RDD[(Int,Point)] = vectors.map(p => (findClosest(p, means), p))
    val closestGrouped:RDD[(Int, Iterable[Point])]= closest.groupByKey()

    def frequencies(points:Iterable[Point]):Map[LangIndex,Int] = {
      val empty:Map[LangIndex,Int] =  (0 until langs.size).map((_,0)).toMap

      def countFreq(frequency:Map[LangIndex,Int], point:Point):Map[LangIndex,Int] = 
        point match {
          case (language,_) => frequency + (language -> (frequency(language)+1))
        }
      
      points.foldLeft(empty)(countFreq)    
    }

    def computeMedianScore(points:Iterable[Point]) : Int  = {
      val sortedPoints = points.toArray.sortBy(point => point._2) 
      val middle:Int = (sortedPoints.size)/2

      if(sortedPoints.size % 2  == 0 ) { 
        (sortedPoints(middle)._2 + sortedPoints(middle+1)._2)/2
      } else {
        (sortedPoints(middle)._2)
      }
    }

    val median = closestGrouped.mapValues { points: Iterable[Point] =>

      val langaugeFrequency: Map[LangIndex, Int] = frequencies(points)
      val sortedFrequency = langaugeFrequency.toList.sortBy(-1 * _._2)

      val mostFrequent = (sortedFrequency.head)

      val numPoints = points.size
      
      // Most common language in the cluster.
      val langLabel: String   = langs(mostFrequent._1)

      // Percent of the questions in the most common language.  
      val langPercent: Double = 100d  * ((mostFrequent._2) / sortedFrequency.size)
      val clusterSize: Int    = numPoints
      val medianScore: Int    = computeMedianScore(points)  // median score

      (langLabel, langPercent, clusterSize, medianScore)
    }

    median.collect().map(_._2).sortBy(_._4)
  }



  def printResults(results: Array[(String, Double, Int, Int)]): Unit = {
    println("Resulting clusters:")
    println("  Score  Dominant language (%percent)  Questions")
    println("================================================")
    for ((lang, percent, size, score) <- results)
      println(f"${score}%7d  ${lang}%-17s (${percent}%-5.1f%%)      ${size}%7d")
  }

}
