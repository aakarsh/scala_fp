package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {


  test("verticalStrips size") {
    val src = new Img(64, 32)
    assert(src.verticalStrips(64).size == 64)
  }

  test("horizontalStrips size") {
    val w = 64
    val h = 32
    val src = new Img(w, h)
    //println("horizontalStripssize :"+src.horizontalStrips(32).size)
    assert(src.horizontalStrips(32).size == 32)
  }
  
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; 
         y <- 0 until 5)
    src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5;          
         y <- 0 until 5)
    assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
             "boxBlurKernel(_,_,0) should be identity.")
  }


  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    //
    src(0, 0) = 0;  src(1, 0) = 1;  src(2, 0) = 2
    src(0, 1) = 3;  src(1, 1) = 4;  src(2, 1) = 5
    src(0, 2) = 6;  src(1, 2) = 7;  src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16
    //
    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
           s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    // 
    val src = new Img(w, h)
    val dst = new Img(w, h)
    // 
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)
    // 
    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected, s"(destination($x, $y) should be $expected)")
    // 
    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    // 
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    // 
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)

  }

  /**
  test("Vertical access count for 32x64 image with parallel blur ")  {

    val w = 32
    val h = 64
 
    val src = new Img(w, h)
    val dst = new Img(w, h)

    VerticalBoxBlur.parBlur(src,dst,32,1)

    var unAccessedPositions   = List[(Int,Int)]()

    for(i <- 0 until dst.width; 
        j <- 0 until dst.height if dst.getAccessCount(i,j)!=1)
    unAccessedPositions = (i,j)::unAccessedPositions

    var accessedPositions   = List[(Int,Int)]()

    for(i <- 0 until dst.width; 
        j <- 0 until dst.height if dst.getAccessCount(i,j)==1)
    accessedPositions = (i,j) :: accessedPositions
    
    //println("accessedPositions: num" + accessedPositions.size) // 128
    accessedPositions.foreach({
      case (x,y) => 
        //println("x,y:"+ dst(x,y) + s"($x,$y)")
    })

    //println("unAccessedPositions: num" + unAccessedPositions.size)
    unAccessedPositions.foreach({
      case (x,y) => 
        //println("x,y: "+dst(x,y) + s" ($x,$y)")
    })

    for(i <- 0 until dst.width; 
        j <- 0 until dst.height )
    assert(dst.getAccessCount(i,j) == 1, 
           s"destination($i,$j) is ${dst(i,j)} but should be 1 ")
  }
  */
  
  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3

    val src = new Img(w, h)
    val dst = new Img(w, h)
    
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    // From: [0,3] - radius = 2
    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")
    /**
    for(i <- 0 until dst.width; 
        j <- 0 until dst.height)
    assert(dst.getAccessCount(i,j) == 1, s"destination($i,$j) is ${dst(i,j)} but should be 1 ")
    */
    
    // Expected:
    //+---------+
    //| 4 5 5 6 |
    //| 4 5 5 6 | 
    //| 4 5 5 6 |
    //+---------+
     
    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)

    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)

    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)

  }

}
