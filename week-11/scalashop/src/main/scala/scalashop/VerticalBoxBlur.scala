package scalashop

import org.scalameter._
import common._
import java.util.concurrent.ForkJoinTask

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {

    val radius = 3
    val width = 1920
    val height = 1080

    val src = new Img(width, height)
    val dst = new Img(width, height)

    //val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    //}
    //println(s"sequential blur time: $seqtime ms")

    val numTasks = 32

    //val partime = standardConfig measure {
    VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    //}
    //println(s"fork/join blur time: $partime ms")
    //println(s"speedup: ${seqtime / partime}")
  }
}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /**
   * Blurs the columns of the source image `src` into the destination image
   * `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   * Within each column, `blur` traverses the pixels by going from top to
   * bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var i = 0 
    var j = 0 

    try{

      for (x <- from until end;         // left to right
           y <- 0    until src.height){   // top  to bottom
        dst(x, y) = boxBlurKernel(src, x, y, radius)

        i = x 
        j = y

      }
    } catch {
      case e:ArrayIndexOutOfBoundsException => 
        throw e
    }
  }


  /**
   * Blurs the columns of the source image in parallel using
   * `numTasks` tasks.  Parallelization is done by stripping the
   * source image `src` into `numTasks` separate strips, where each
   * strip is composed of some number of columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {

    def toBlurTask(region: (Int, Int)): ForkJoinTask[Unit] =
      region match {
        case (start, end) => task(blur(src, dst, start, end, radius))
      }

    // pair of strip borders of length numTasks, each of width - stripSize
    val borders = src.verticalStrips(numTasks)

    // Invoke task constructs in parallel and wait for them to finish.
    borders.map(toBlurTask).map(_.join)
  }

}
