package quickcheck

/**
 * Optimal Purely Functional Priority Queues
 *
 * ~ {Gerth Stølting Brodal,
 *    Chris Okasaki}
 *
 * http://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf
 */

// Figure 1, page 3
import common._

/**
 * Heap of integers.
 */
trait IntHeap extends Heap {
  override type A = Int
  override def ord = scala.math.Ordering.Int
}

/**
 * Basic interface that heap implementations must guarantee. Uses
 * generic type for the type of the heap element.
 */
trait Heap {
  type H                     // Type of a heap
  type A                     // Type of an element

  def ord: Ordering[A]       // Ordering on elements
  def empty: H               // The empty heap

  def isEmpty(h: H): Boolean // Whether the given heap h is empty

  def insert(x: A, h: H): H  // The heap resulting from inserting x into h
  def meld(h1: H, h2: H): H  // The heap resulting from merging h1 and h2

  def findMin(h: H): A       // A minimum of the heap h
  def deleteMin(h: H): H     // A heap resulting from deleting a minimum of h
}

// {Figure: 3 }- {Page 7}
trait BinomialHeap extends Heap {

  type Rank = Int

  /**
   * x - An orderable element.
   * r - rank/height of the tree. The ranks of the tree always multiple's of (2^i).
   * c - list of child nodes
   */
  case class Node(x: A, r: Rank, c: List[Node])

  /**
   * a binomial heap is a list of binomial tree nodes
   * sorted ascending order of ranks.
   */
  override type H = List[Node]

  /**
   * root element : also the minimum element of the tree.
   */
  protected def root(t: Node) = t.x

  /**
   * rank of the node: the height of the tree cached and updated
   */
  protected def rank(t: Node) = t.r

  /**
   * Link the trees of equal rank make the larger one the
   * left child of the smaller one.
   */
  protected def link(t1: Node, t2: Node): Node = // t1.r == t2.r
    // ( t1.x <= t2.x )  - use the smaller i.e.
    //  if t1.x is the smaller hence make it the
    //  new root element preserving the root property.
    if (ord.lteq(t1.x,t2.x))
      Node ( t1.x,        // - picked as the root element
             t1.r + 1,    // - The height of the tree is increased by 1
                          // - and the number of nodes is doubled.
             t2 :: t1.c)  // - t1. was picked as the root. Append t2 the bigger one
                          // - as the left most child of of t1

    // Make: t2 root the head
    else Node ( t2.x,         // Same as above but now pick t2 as the root
                t2.r + 1 ,    
                t1 :: t2.c )

  /**
   * insert node t into binomial queue.  Preserving the property that
   * the queue is sorted in increasing order of the rank.
   *
   * requires that the rank of tree t less than equal to head of the heap. 
   */
  protected def ins(t: Node, ts: H): H =
    ts match {

      /**
       * If queue is empty then tree t will be
       * the only element in the heap.
       */
      case Nil => List(t)

      // t.r <= tp.r - why can we assume this ?

      case tp::ts => // tp - is the (smallest/head) element in the heap.

        /**
         * if the rank of the element to be inserted is less than the
         * rank of head of the heap. then it can safely be inserted
         * before the head element preserving the ascending rank
         * property.
         */
        if ( t.r < tp.r ) t :: tp :: ts

        /**
         * If the rank is equal to head then we link the head and tree
         * the new node is clearly the smallest ranked element and can
         * be inserted using recursive call to ins.
         */
        else ins( link(t, tp), ts)

    }

  /**
   * Empty heap is just the empty list of nodes.
   */
  override def empty = Nil

  /**
   * if the binomial queue is empty then true.
   */
  override def isEmpty(ts: H) = ts.isEmpty

  /**
   * insert the element x into the heap ts. the insertion will proceed
   * like a carry in ascendeing order and causes trees which overflow
   * their rank to get melded into their neighbors.
   */
  override def insert(x: A, ts: H) = ins ( Node(x,0,Nil) , ts)

  /**
   * Returns the meld of two queues by matching their ranks.
   */
  override def meld(ts1: H, ts2: H) = (ts1, ts2) match {

    // Meld of empty list and a full list is simply the other list.
    case (Nil, ts) => ts
    case (ts, Nil) => ts

    // The ts1 and ts2 are organized in increazing order of ranks
    case (t1::ts1, t2::ts2) =>
      // Melding proceeds along the head of lists. If their ranks are unequal the minimum rank
      // property of the head of the list is preserved.
      if (t1.r < t2.r)      t1::meld(ts1,t2::ts2)
      else if (t2.r < t1.r) t2::meld(t1::ts1,ts2)

      // Oops, the rank of head binomial tree of both lists are equal
      // are equal.
      else ins ( link(t1,t2), meld(ts1,ts2))
  }

  /**
   * Find the minimum element in the binomial queue
   */
  override def findMin(ts: H) = ts match {
    case Nil =>
      throw new NoSuchElementException("min of empty heap")
    case t::Nil => root(t)
    case t::ts  =>
      val x = findMin(ts)
      if (ord.lteq(root(t),x)) root(t) else x
  }

  /**
   *
   */
  override def deleteMin(ts: H) = ts match {

    case Nil =>
      throw new NoSuchElementException("delete min of empty heap")

    case t::ts =>
      def getMin(t: Node, ts: H): (Node, H) =
        ts match {
          case Nil     => (t, Nil)
          case tp::tsp =>
            val (tq,tsq) = getMin(tp, tsp)
            if (ord.lteq(root(t),root(tq))) (t,ts)
            else (tq,t::tsq)
        }
      val (Node(_,_,c),tsq) = getMin(t, ts)
      meld(c.reverse, tsq)
  }

}

trait Bogus1BinomialHeap extends BinomialHeap {
  // Breaks findMin by always returning the minimum from first element.
  override def findMin(ts: H) =
    ts match {
      case Nil => throw new NoSuchElementException("min of empty heap")
      case t::ts => root(t)
    }
}

trait Bogus2BinomialHeap extends BinomialHeap {
  // Breaks link by ???
  override protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (!ord.lteq(t1.x,t2.x)) Node(t1.x, t1.r+1, t2::t1.c)
    else Node(t2.x, t2.r+1, t1::t2.c)

}

trait Bogus3BinomialHeap extends BinomialHeap {
  // Breaks link by ???
  override protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (ord.lteq(t1.x,t2.x)) Node(t1.x, t1.r+1, t1::t1.c)
    else Node(t2.x, t2.r+1, t2::t2.c)
}

trait Bogus4BinomialHeap extends BinomialHeap {
  // Breaks deleteMin by ???
  override def deleteMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t::ts => meld(t.c.reverse, ts)
  }
}

trait Bogus5BinomialHeap extends BinomialHeap {
  // Breaks meld by ???
  override def meld(ts1: H, ts2: H) = ts1 match {
    case Nil => ts2
    case t1::ts1 => List(Node(t1.x, t1.r, ts1++ts2))
  }

}
