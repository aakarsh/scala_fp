package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

import FloatOps._

  // test cases for quad tree


  test("test recursive forking for a square of size 10") {

    val (cX, cY) = (5.0f, 5.0f)
    val delta    =  2.5f
    val size     =  10.0f
    val speed    =  0.0f  // still

    val quadCenters =
      for(i <- List(-1,1);
          j <- List(-1,1)) // delta
      yield (cX + (i * delta),
             cY + (j * delta))

    println("Centers : " + quadCenters)

    val bodies = quadCenters.map( { case (x,y) =>  new Body(1f, x, y, speed, speed) })

    println(s"Bodies: ${bodies}")

    var tree : Quad = new Empty(cX, cY, size)

    var i = 0
    for (body <- bodies) {
      println("Insert Body :" + body)
      // Failed after second insert
      tree = tree.insert(body)
      println(s"tree-$i : ${tree}")
      i += 1
    }

    // Tree Something.
    println("Tree:\n" + tree)
  }



  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {

    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f") // Failing her.
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {

    val b = new Body(123f, 18f, 26f, 0f, 0f)
    // leaves of size 5
    val nw = Leaf(17.5f, 27.5f,  5f, Seq(b)) 
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)

    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")


    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")


  }


  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  // something.
  test("Body.updated should take bodies in a Leaf into account") {

    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)

  }


  test("Fork.insert(b) should insert recursively in the appropriate quadrant" ) {

  }

  // test cases for sector matrix
  /**
   * PASS
   */
  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    // (x = 25, y = 47)
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()

    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97

    // Sector Precision =  8
    // We have a 64-buckets, 8 in each row [ (/ 97 8) = 12 ]
    // From (1,1) to (97, 97) -- which means we will have 12 cells per row
    // Creating the boundaries
    // (/ 25 12) = 2
    // (/ 47 12) = 3

    // Thus we expect the ConcBuffer at (2,3) to contain the added body
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }


  test ("fork with 4 empty quadrants.") {
    /**
     * Q: How does on create a fork here ?
     * Q: Without autocomplete this is just too much of a pain ?
     */
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val bodies = (0 to 10).map((x:Int) =>
      new Body(10f, (10.0f * x), (5.0f *x), 0f, 0f))

    var insertionResult: Quad = new Empty(0f,0f ,500f)

    var i = 0
    for(b <- bodies ) {
      i += 1
      if(i <= 2)
        insertionResult = insertionResult.insert(b)
    }

    def isFork(b : Quad) : Boolean = b match {
      case Empty(_,_,_)    => false
      case Fork (_,_,_,_)  => true
      case Leaf (_,_,_,_)  => false
    }

    assert(isFork(insertionResult),
           s"Expected fork but was ${insertionResult}")
  }

  // [insert] - I changed the <=  to <
  test("'insert' should work correctly on a leaf with center (1,1) and size <= minimumSize ")
  {

    val centerBody = new Body(123f, 1f, 1f, 0f, 0f)
    val leaf = new Leaf( 1f, 1f, 0.00001f, List(centerBody))

    val root = leaf
    var newLeaf:Quad = leaf

    for(i <- 0 until 10) {
      val newBody = new Body(i * 10f, 1f, 1f, 0f, 0f)
      newLeaf = newLeaf.insert(newBody)
      println(newLeaf)
    }

    assert(newLeaf match {
      case Leaf(_,_,_, bodies) => (bodies.size == 11)
      case _ => false
    }, s"${newLeaf} is not a leaf of size 11 ")

  }



  // Leaf.insert - should return a new Fork
  test("Leaf.insert(b) should return a new Fork if size > minimumSize" ) {

  }



  test("'insert' should work correctly on a leaf with center (1,1) and size 2" ) {

  }

  test("computeSectorMatrix should be parallel" ) {

  }



  test("'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector" ) {

  }



  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector" ) {

  }

  test("Body.updated should recursively traverse a Fork close to it" ) {

  }

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100" ) {

  }

  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)" ) {

  }

  test("'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96" ) {

  }


/**
 * TODO Need to go through and fix these.
 *
 * But I need a break or at least a change in subject matter.
 *
 * [Test Description] Fork with 4 empty quadrants
 * [Observed Error] NaN did not equal 20.0 NaN should be 20f
 * [Lost Points] 2
 *
 * [Test Description] 'insert' should work correctly on a leaf with center (1,1) and size <= minimumSize
 * [Observed Error] 2
 * [exception was thrown] detailed error message in debug output section below
 * [Lost Points] 2
 *
 * [Test Description] Leaf.insert(b) should return a new Fork if size > minimumSize
 * [Observed Error] 2
 * [exception was thrown] detailed error message in debug output section below
 * [Lost Points] 2
 *
 * [Test Description] computeSectorMatrix should be parallel
 * [Observed Error] 72
 * [exception was thrown] detailed error message in debug output section below
 * [Lost Points] 2
 *
 * [Test Description] Fork.insert(b) should insert recursively in the appropriate quadrant
 * [Observed Error] 2
 * [exception was thrown] detailed error message in debug output section below
 * [Lost Points] 2
 *
 * [Test Description] 'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector
 * [Observed Error] ConcBuffer() had size 0 instead of expected size 1 bucket (0,2) should have size 1
 * [Lost Points] 2
 *
 * [Test Description] 'insert' should work correctly on a leaf with center (1,1) and size 2
 * [Observed Error] 2
 * [exception was thrown] detailed error message in debug output section below
 * [Lost Points] 2
 *
 * [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector
 * [Observed Error] ConcBuffer(barneshut.package$Body@1fa268de) had size 1 instead of expected size 2 bucket (6,1) should have size 2
 * [Lost Points] 2
 *
 * [Test Description] Body.updated should recursively traverse a Fork close to it
 * [Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(0.2641816735267639) was false xspeed was 0.26652554
 * [Lost Points] 2
 *
 * [Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
 * [Observed Error] res was false Body not found in the right sector. Hint: sector sizes could be fractions
 * [Lost Points] 2
 *
 * [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)
 * [Observed Error] res was false Body 3 not found in the right sector in combined sector matrix
 * [Lost Points] 2
 *
 * [Test Description] 'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96
 * [Observed Error] 72
 * [exception was thrown] detailed error message in debug output section below
 * [Lost Points] 2
 */

}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}
