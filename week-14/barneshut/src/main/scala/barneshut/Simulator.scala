package barneshut

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import scala.collection.parallel.TaskSupport
import scala.collection.parallel.Combiner
import scala.collection.parallel.mutable.ParHashSet
import common._

class Simulator(val taskSupport: TaskSupport,
                val timeStats  : TimeStatistics) {


  /**
   * Since boundaries will change for each body after they have been
   * moved all boundaries will need to be recomputed at each step.
   */

  /**
   * Updates the boundaries to include the body.
   */
  def updateBoundaries(boundaries: Boundaries, body: Body): Boundaries = {
    // [updateBoundaries] - compute min

    boundaries.minX = math.min(boundaries.minX, body.x)
    boundaries.minY = math.min(boundaries.minY, body.y)
    // compute max
    boundaries.maxX = math.max(boundaries.maxX, body.x)
    boundaries.maxY = math.max(boundaries.maxY, body.y)

    boundaries
  }

  /**
   * [mergeBoundaries]
   */
  def mergeBoundaries(a: Boundaries,
                      b: Boundaries): Boundaries = {
    // [mergeBoundaries]
    val  enclosing = new Boundaries()
    enclosing.minX = math.min(a.minX,b.minX)
    enclosing.minY = math.min(a.minY,b.minY)

    enclosing.maxX = math.max(a.maxX,b.maxX)
    enclosing.maxY = math.max(a.maxY,b.maxY)

    enclosing
  }

  // [computeBoundaries]
  def computeBoundaries(bodies: Seq[Body]): Boundaries =
    timeStats.timed("boundaries") {
      // parallel bodies.
      val parBodies = bodies.par
      parBodies.tasksupport = taskSupport
      parBodies.aggregate(new Boundaries)(updateBoundaries, mergeBoundaries)
    }

  /**
   * Uses {SECTOR_PRECISION}.
   */
  def computeSectorMatrix(bodies: Seq[Body],
                          boundaries: Boundaries): SectorMatrix =
    timeStats.timed("matrix") {
      // start with parallel bodies ...
      val parBodies = bodies.par
      parBodies.tasksupport = taskSupport
      // parBodies.aggregate( new SectorMatrix() )
      ???
    }

  /**
   * Takes as input a SectorMatrix  and constructs a QuadTree
   * representing the region
   */
  def computeQuad(sectorMatrix: SectorMatrix): Quad =
    timeStats.timed("quad") {
      sectorMatrix.toQuad(taskSupport.parallelismLevel)
    }

  /**
   * Takes a sequence of bodies and an existing quad tree
   * and computes forces on each body wrt other bodies,
   * updating their position. Uses aggregate to update,merge
   * list of bodeis
   */
  def updateBodies(bodies: Seq[Body], quad: Quad): Seq[Body] =
    timeStats.timed("update") {
      import scala.collection.immutable.List

      val parBodies = bodies.par
      parBodies.tasksupport = taskSupport

      def update (acc: List[Body], b: Body) : List[Body] = b.updated(quad) :: acc
      def merge  (b1: List[Body], 
                  b2: List[Body]) : List[Body] = b1 ::: b2

      val empty = List[Body]()
      
      parBodies.aggregate(empty)(update,merge)
    }

  def eliminateOutliers(bodies: Seq[Body],
                        sectorMatrix: SectorMatrix,
                        quad: Quad): Seq[Body] =

    timeStats.timed("eliminate") {

      def isOutlier(b: Body): Boolean = {
        val dx = quad.massX - b.x
        val dy = quad.massY - b.y
        val d = math.sqrt(dx * dx + dy * dy)

        // object is far away from the center of the mass
        if (d > eliminationThreshold * sectorMatrix.boundaries.size) {

          val nx = dx / d
          val ny = dy / d

          val relativeSpeed = b.xspeed * nx + b.yspeed * ny

          // object is moving away from the center of the mass
          if (relativeSpeed < 0) {
            val escapeSpeed = math.sqrt(2 * gee * quad.mass / d)
            // object has the espace velocity
            - relativeSpeed > (2 * escapeSpeed)
          } else false
        } else false
      }

    def outliersInSector(x: Int, y: Int): Combiner[Body, ParHashSet[Body]] = {
      val combiner = ParHashSet.newCombiner[Body]
      combiner ++= sectorMatrix(x, y).filter(isOutlier)
      combiner
    }

    val sectorPrecision = sectorMatrix.sectorPrecision
    val horizontalBorder = for (x <- 0 until sectorPrecision;
                                y <-  Seq(0, sectorPrecision - 1))
                           yield (x, y)

    val verticalBorder = for (y <- 1 until sectorPrecision - 1;
                              x <-  Seq(0, sectorPrecision - 1))
                         yield (x, y)

    val borderSectors = horizontalBorder ++ verticalBorder

    // compute the set of outliers
    val parBorderSectors = borderSectors.par

    parBorderSectors.tasksupport = taskSupport

    val outliers = parBorderSectors.map(
      { case (x, y) => outliersInSector(x, y) }
    ).reduce(_ combine _).result

    // Filter the bodies that are outliers.
    val parBodies = bodies.par
    parBodies.filter(!outliers(_)).seq

  }

  def step(bodies: Seq[Body]): (Seq[Body], Quad) = {

    // 1. Compute boundaries.
    val boundaries = computeBoundaries(bodies)

    // 2. Compute sector matrix.
    val sectorMatrix = computeSectorMatrix(bodies, boundaries)

    // 3. Compute quad tree.
    val quad = computeQuad(sectorMatrix)

    // 4. Eliminate outliers.
    val filteredBodies = eliminateOutliers(bodies, sectorMatrix, quad)

    // 5. Update body velocities and positions.
    val newBodies = updateBodies(filteredBodies, quad)

    (newBodies, quad)
  }

}
