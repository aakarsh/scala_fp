import common._
import barneshut.conctrees._

package object barneshut {

  /**
   * Boundaries -
   *
   */
  class Boundaries {

    var minX = Float.MaxValue
    var minY = Float.MaxValue
    var maxX = Float.MinValue
    var maxY = Float.MinValue

    def width =
      maxX - minX

    def height =
      maxY - minY

    def size =
      math.max(width, height)

    def centerX =
      minX + width / 2

    def centerY =
      minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float
    def massY: Float
    def mass: Float
    def centerX: Float
    def centerY: Float
    def size: Float
    def total: Int
    def insert(b: Body): Quad
  }

  /**
   * In the quad tree an Empty node represents a region not containing any
   * sub Quads and not containing any bodies.
   */
  case class Empty(centerX : Float,
                   centerY : Float,
                   size    : Float) extends Quad {
    /**
     * Since there is no internal bodies its center of mass
     * is considered to be same as its center.
     */
    def massX: Float = centerX
    def massY: Float = centerY

    def mass:  Float = 0
    def total: Int   = 0

    /**
     * Inserting into an Empty Quad tree node
     * will cause it to change into a Leaf.
     */
    def insert(b: Body): Quad = new Leaf(centerX,centerY,size,List(b))
  }
  
  /**
   * Fork 
   */
  case class Fork(nw: Quad, 
                  ne: Quad, 
                  sw: Quad, 
                  se: Quad) extends Quad {

    val centerX: Float = nw.centerX + ( nw.size / 2)
    val centerY: Float = nw.centerY + ( nw.size / 2)
    val size:    Float = nw.size * 2 // assuming dqual size of all four quads
    val mass:    Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX:   Float = (nw.mass * nw.massX) + (ne.mass * ne.massX) + (sw.mass * sw.massX) + (se.mass * se.massX)
    val massY:   Float = (nw.mass * nw.massY) + (ne.mass * ne.massY) + (sw.mass * sw.massY) + (se.mass * se.massY)
    val total:   Int   = nw.total + ne.total + sw.total + se.total

    /**
     * Inserting into a fork is a recursive process.
     */
    def insert(b: Body): Fork = {      
      (b.x,b.y)  match {
        case (x, y) if (x <= centerX && y <= centerY) => new Fork(nw.insert(b),ne,sw,se)
        case (x, y) if (x >  centerX && y <= centerY) => new Fork(nw,ne.insert(b),sw,se)
        case (x, y) if (x <= centerX && y >  centerY) => new Fork(nw,ne,sw.insert(b),se)
        case (x, y) if (x >  centerX && y >  centerY) => new Fork(nw,ne,sw,se.insert(b))
      }
    }
  }

  /**
   * Leaf is a squence of bodies.
   *
   * If the size of the Leaf is greater
   * than a predefined constant minimumSize.
   *
   * Inserting into the leaf node will cause it
   * to turn into a fork. If the size is small
   * enough additional inserts will certainly cause
   * the leaf to turn into a fork on the
   * second insert.
   *
   */
  case class Leaf(centerX: Float, centerY: Float,
                  size: Float, bodies: Seq[Body]) extends Quad {

    // total-mass of all bodies.
    def totalMass(bodies:Seq[Body]):Float =
      bodies.map(_.mass).sum

    // dot-product of two vectors.
    def dot(v1:Seq[Float], v2:Seq[Float]): Float = (v1,v2).zipped.map(_*_).sum

    /**
     * Mass is the sum of masses of all bodies, massX and massY
     * represent the inertial centers of respective bodies.
     */
    val (mass, massX, massY) = (totalMass(bodies),
                                // m_x - inertial mass along the x coordinate
                                dot(bodies.map(_.mass), bodies.map(_.x)),
                                // m_y - inertial mass along the y coordinate
                                dot(bodies.map(_.mass), bodies.map(_.y)))

    // total number of bodies.
    val total: Int = bodies.size

    /**
     * While inserting a body into the leaf we check the leaf size.
     * if it is greater than the minimum size then we will insert
     */
    def insert(body: Body): Quad = {
      // split leaf
      if( size >= minimumSize ) {

        val delta   = size / 4
        val quadLen = size / 2

        // [fork]-Map: To a particular sector: { nw-0, ne-1, se-2 , sw-3 }
        def sector(b: Body): Int = {
          (b.x,b.y) match {
            case (x,y) if (x <= centerX && y <= centerY) => 0 // NW
            case (x,y) if (x >  centerX && y <= centerY) => 1 // NE
            case (x,y) if (x <= centerX && y >  centerY) => 2 // SE
            case (x,y) if (x >  centerX && y >  centerY) => 3 // SW
          }
        }

        // partition the objects into (one of four) coordinates spaces.
        // compute where in the partitioned space the body will lie
        // {nw-0, ne-1, se-2 , sw-3}
        val groupMap : Map[Int, Seq[Body]] = (body::bodies.toList).groupBy(sector)

        // Create list of bodies sorted by sector in order {nw, ne, se, sw}
        val groups = groupMap.toList.sortBy(_._1).map({case (sec, bodies) => bodies})

        // [Maybe better way] : (-,-):0,(+,-):1,(-,+):2,(+,+):3
        // Construct deltas from current center to center of the four new quads.
        val deltas  =
          for(i <- List(-1,1);
              j <- List(-1,1)) yield (j * delta, i * delta)

        val centers = deltas.map(d => (centerX + d._1 ,centerY + d._2))
        val centerGroups = (centers, groups).zipped

        val nodes =
          centerGroups.map(
            { case (c,g) if g.isEmpty =>  new Empty(c._1, c._2, quadLen)
              case (c,g) => new Leaf(c._1,c._2, quadLen, g)
            })
        new Fork(nodes(0), nodes(1), nodes(2), nodes(3))
      } else {
        new Leaf(centerX,centerY,size,body::bodies.toList)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  /**
   * Compute the gravitational force between two bodies. 
   */
  def force(m1: Float, m2: Float, dist: Float): Float =
    gee * m1 * m2 / (dist * dist)
  
  /**
   * Euclidean distance between (x0,y0) and (x1,y1)
   */
  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt( (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float,
             val x: Float,
             val y: Float,
             val xspeed: Float,
             val yspeed: Float) {

    /**
     * Compute the updated position of the body based on traversing
     * QuadTree which provides an efficient representation of all
     * other bodies in QuadTree.
     */
    def updated(quad: Quad): Body = {

      var netforcex = 0.0f
      var netforcey = 0.0f

      /**
       * Compute the force exerted by a particular mass and add it to
       * the netforce being computed.
       */
      def addForce(thatMass: Float, 
                   thatMassX: Float, thatMassY: Float): Unit = {

        // euclidiean distance between the masses.
        val dist = distance(thatMassX, thatMassY, x, y)

        /**
         * 
         * If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * 
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         *
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) { 

          // Net force on this body exerted by that mass.
          val dforce = force(mass, thatMass, dist)
          
          // Direction of the force
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist

          // Force component in x direction
          val dforcex = dforce * xn
          // Force component in the y direction.
          val dforcey = dforce * yn

          // Each mass will add to the total-force in (F_x and F_y)
          // directions which will apply to current object.
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      /**
       * Traverse the quad tree using summarizing masses for
       * far away bodies.
       */
      def traverse(quad: Quad): Unit = (quad: Quad) match {          
        case Empty(_, _, _) =>          
        // no body, no force        
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          addForce(quad.mass,quad.massX,quad.massY)

        case Fork(nw, ne, sw, se) =>           
          // Determine whether we need to use a threshold.
          val dist = distance(quad.massX, quad.massY, x, y)
          if(quad.size / dist < theta) { // dont recurse if too far
            addForce(quad.mass,quad.massX,quad.massY)
          } else {
            List(nw,ne,sw,se).map(traverse)
          }
      }
      
      traverse(quad)

      // Use old speed to compute new poistion of the body
      // delta represents the fractional time interval 
      val nx = x + xspeed * delta
      val ny = y + yspeed * delta

      /**
       * a = f/m : Used to compute new speed
       * Use new acceleration to compute new speed of the object.
       */
      val nxspeed = xspeed + (netforcex / mass) * delta
      val nyspeed = yspeed + (netforcey / mass) * delta

      // return body with new position and new velocity
      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  /**
   * Sector matrix is the biggest square border which includes all
   * the boundary rectangles.  A sector matrix will contain
   * (sectorPrecision * sectorPrecision) cells.
   *
   * Internally sector matrix represents the sector cells as a linear array
   * of CocnBuffer[Body] where concbuffer can be considered as a buffer
   * with efficient parallel list implementation of list of Body objects.
   *
   * Together the ConcBuffer[Body] of a particular cell contains list of all
   * body objects present in that cell.
   *
   */
  class SectorMatrix(val boundaries: Boundaries,
                     val sectorPrecision: Int) {

    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer


    /**
     * Used to add a body to SectorMatrix :
     *
     * 1. Compute the appropriate cell in which to add the body.
     * 2. Use the underlying ConcBuffer[Body] to add the object.
     * 3. 
     */
    def +=(b: Body) : SectorMatrix = {
      /**
       * size - Length of each sector.
       */
      def sector(x: Float, min: Float, max: Float, size: Float = sectorSize): Int = {
        // Round x to min or max if it exceeds either one of them.
        val v =
          if(x < min) min
          else if( x > max) max
          else x

        // Find the correct sector
        (v/size).toInt
      }

      var (secX:Int, secY:Int) = (sector(b.x, boundaries.minX, boundaries.maxX),
                                  sector(b.y, boundaries.minY, boundaries.maxY))

      this(secX, secY) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    /**
     * Combine this and that sector matrix.
     */
    def combine(that: SectorMatrix): SectorMatrix = { // AN:
      // go through ConcBuffer lists and combine them
      for(i <- 0 until matrix.length)
        matrix(i) = this.matrix(i).combine(that.matrix(i))
      this
    }

    /**
     * Takes a parallelism threshold and goes on to
     * do the balancing.
     */
    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }
      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
