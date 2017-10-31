
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

    def toHexTable = {
      for(j <- 0 until height)
       yield
         for(i <- 0 until width)
          yield "Ox%08X".format(apply(i,j))
    }

    def printTable():Unit = {
      this.toHexTable.foreach(row => println(row.reduce(_+" "+_)))
    }
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    def toChannels(p: RGBA): Vector[Int] =
      Vector(red(p),green(p), blue(p), alpha(p))

    def toPixel(l:Vector[Int]): RGBA = {
      val channelValues = l.map(clamp(_,0,255))
      rgba(channelValues(0),
           channelValues(1),
           channelValues(2),
           channelValues(3))
    }

    val neighbourChannels =
      for(i  <- (x - radius) to (x + radius) if i >= 0 && i < src.width;
          j  <- (y - radius) to (y + radius) if j >= 0 && j < src.height) yield {
        
        val r = clamp(i,0, src.width -1)
        val c = clamp(j,0, src.height-1)

        // get the pixel
        val pixel = src(r,c)
        // decompose into a .list.of.channels.
        toChannels(pixel)
      }
      
    def listSum(lls: Seq[Seq[Int]]) : Vector[Int] =
      lls.reduce((ls,acc) => (ls, acc).zipped.map(_ + _)).toVector
    
    val channelTotal : Vector[Int] = listSum(neighbourChannels)
    val area  = neighbourChannels.size
    
    val averageChannelValeus:Vector[Int] =
      channelTotal.map( total => (total/area).toInt)
    
    toPixel(averageChannelValeus)
  }

}
