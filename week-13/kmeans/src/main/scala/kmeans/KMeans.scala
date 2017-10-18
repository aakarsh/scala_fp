package kmeans

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

class KMeans {

  def generatePoints(k: Int,
                     num: Int): Seq[Point] = {
    // Random: (x,y,z)
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)

    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int,
                      points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ =>
      points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  /**
   * Given a sequence of means and a point determine
   * the closest mean for this point. Each point's
   * determination of the closest mean is independent
   * of all other points. Thus much of this can be
   * done in parallel for some definition of
   * parallel.
   */
  def findClosest(p: Point,
                  means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)

    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    // Pick the closest mean to the given point.
    closest
  }

  /**
   * Classify possibly in parallel sequence of points
   * to their closest means. Since each classificaiton
   * is independent of others, that is each point can
   * be classified fully independently of the
   * other points near the means.
   */
  def classify(points: GenSeq[Point],
               means:  GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    /**
     * Map point to its closest mean.
     */
    val map  = points.groupBy( (p:Point) => findClosest(p,means))
    /**
     * For means far away from any points use
     * empty sequence point list.
     */
    val absentMeans : GenSeq[Point] = means.filter(! map.contains(_))
    // Point map along with any absent means
    map ++ absentMeans.map((_ -> GenSeq[Point]()))
  }

  /**
   * Average is simply the average of each
   * corresponding coordinate of the point.
   */
  def findAverage(oldMean: Point,
                  points : GenSeq[Point]): Point =
    if (points.length == 0)
      oldMean
    else {
      //  { x_total, y_total, z_total }
      var ( x_total, y_total, z_total ) = (0.0, 0.0, 0.0)
      val n = points.length
      // Each individual point.in: (x, y, z)
      points.seq.foreach {
        ( p:Point ) => {
          x_total += p.x
          y_total += p.y
          z_total += p.z
        }
      }
      new Point(x_total/n, y_total/n, z_total/n)
    }

  /**
   * Take classified set of points for each classified
   * set compute new means average.
   */
  def update(classified: GenMap[Point,   GenSeq[Point]],
             oldMeans:   GenSeq[Point]): GenSeq[Point] = {
    // update ...
    val newMeans = classified.map({
      case (oldMean,points) =>
        (oldMean,findAverage(oldMean, points))
    })

    oldMeans.map({
      (mean:Point) =>
      if(newMeans.contains(mean))
        newMeans(mean)
      else mean // preserve unclassified means
    })
  }

  /**
   * Compare oldMeans and newMeans to check if
   * there exists a difference of at least (eta)
   */
  def converged(eta: Double)
               (oldMeans: GenSeq[Point],
                newMeans: GenSeq[Point] ): Boolean = {
    val diffList =
      oldMeans.zip(newMeans).filter({
        case (oldMean, newMean) =>
          oldMean.squareDistance(newMean) > eta
      })    
    !(diffList.size > 0)
  }

  /**
   *
   */
  @tailrec
  final def kMeans(points: GenSeq[Point],
                   means:  GenSeq[Point],
                   eta: Double): GenSeq[Point] = {
    // Classify points into new means
    val newClassification =  classify(points,means)
    val newMeans = update(newClassification, means)
    if (!converged(eta)(means,newMeans)) {
      kMeans(points, newMeans, eta)
    } else {
      newMeans
    }
   }
}

/**
 * Describes one point in three-dimensional space.
 *
 * Note: deliberately uses reference equality.
 *
 */
class Point(val x: Double, val y: Double, val z: Double) {

  private def square(v: Double): Double = v * v

  /**
   * Compute the distance between the current point
   * and the other point.
   */
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }

  private def round(v: Double): Double = (v * 100).toInt / 100.0

  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"

}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {

    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32

    val points = kMeans.generatePoints  (k, numPoints)
    val means  = kMeans.initializeMeans (k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }

    println(s"Sequential time: $seqtime .ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }

    println(s"Parallel Time: ${partime} ms")
    println(s"Speedup: ${seqtime / partime}")

  }

}
