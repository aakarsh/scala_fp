
import common._
import scala.language.implicitConversions

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  type Channels = Vector[Int]

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
  
  // Convert pixel to individual channelss
  implicit def pixel2Channels(p: RGBA): Channels =
    Vector(red(p),green(p), blue(p), alpha(p))

    // Values for each channel channel between [0..255]
  implicit def channel2Pixel(channels: Channels): RGBA = {      
    val cv = channels.map(clamp(_,0,255))
    rgba(cv(0), cv(1), cv(2), cv(3))
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    //var accessCount = new Array[Int]( width * height)
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    //def getAccessCount(x:Int, y:Int) :Int = accessCount(y* width+ x)

    def update(x: Int, y: Int, c: RGBA): Unit = {
      data(y * width + x) = c
      //accessCount(y*width+x) += 1
    }
    /**
     * These strips go (up down), along the width.
     */
    def verticalStrips(n:Int) : Seq[(Int,Int)] = {
      val stripSize: Int = clamp((width / n).toInt, 1, width)
      val stripBorders   = ((0         until width by stripSize),
                            (stripSize to    width by stripSize)).zipped
      stripBorders.toList
    }
    /**
     * These stips go (left right) along the height
     */
    def horizontalStrips(n:Int) : Seq[(Int,Int)] = {
      val stripSize: Int = clamp((height / n).toInt,1, height)
      val stripBorders   = ((0         until height by stripSize),
                            (stripSize  to   height by stripSize)).zipped 
      stripBorders.toList
    }

    // All neighbouring cells of an image position.
    def neighbours(x:Int, y:Int, radius:Int) : Seq[RGBA] = {
      for(i  <- (x - radius) to (x + radius) if i >= 0 && i < width;
          j  <- (y - radius) to (y + radius) if j >= 0 && j < height) 
      yield this(i,j)
    }

    def toHexTable = 
      for(j <- 0 until height)
       yield
         for(i <- 0 until width)
          yield "Ox%08X".format(apply(i,j))

    def printTable() : Unit = 
      this.toHexTable.foreach( row => println( row.reduce( _ + " " + _ ) ))
    
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    
    def listSum(lls: Seq[Seq[Int]]) : Vector[Int] =
      lls.foldLeft(Vector(0,0,0,0))( (ls,acc) => (ls, acc).zipped.map(_ + _) ).toVector
    
    // all channels within radius distance from (x,y) square
    // which also lie inside the image.
    val neighbourChannels: Seq[Channels] = 
      src.neighbours(x, y, radius).map(pixel2Channels)
    
    // collect total value of channels 
    val channelTotal : Channels = listSum(neighbourChannels)
    val numChannels  = neighbourChannels.size
    
    val averageChannelValues:Channels =
      channelTotal.map( _ / numChannels).map(_.toInt)

    averageChannelValues
  }

}
