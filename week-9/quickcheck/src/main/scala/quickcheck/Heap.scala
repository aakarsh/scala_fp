package quickcheck

/**
 * Optimal Purely Functional Priority Queues
 *
 * ~ {Gerth Stølting Brodal,
 *    Chris Okasaki}
 *
 * http://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf
 *
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

  def findMin(h: H): A       // A minimum of the heap H
  def deleteMin(h: H): H     // A heap resulting from deleting a minimum of H

  def sortedList(h:H): List[A] = {

    def sorted_aux(acc:List[A], h:H): List[A] = h match {
      case heap if heap == empty  =>  acc
      case _ =>
        val m = findMin(h)
        sorted_aux(m::acc,deleteMin(h))
    }

    sorted_aux(List[A](),h).reverse
  }


}

// {Figure: 3 }- {Page 7}
trait BinomialHeap extends Heap {

  type Rank = Int

  /**
   * x - An orderable element.
   * r - rank/height of the tree. The ranks of the tree always multiple's of (2^i).
   * c - list of child nodes
   */
  case class Node( x: A, r: Rank, c: List[Node])

  /**
   * a binomial heap is a list of binomial tree nodes
   * sorted ascending order of ranks.
   */
  override type H = List[Node] // List of Nodes

  /**
   * root element: Also the minimum element of the tree.
   */
  protected def root(t: Node) = t.x

  /**
   * Rank of the node: The height of the tree cached and updated
   */
  protected def rank(t: Node) = t.r

  /**
   * Link the trees of equal rank make the larger one the left child
   * of the smaller one.
   */
  protected def link(t1: Node,
                     t2: Node): Node = // t1.r == t2.r
    // ( t1.x <= t2.x )  - use the smaller i.e.
    //  if t1.x is the smaller hence make it the
    //  new root element preserving the root property.
    // t1.x < t2.x
    if (ord.lteq(t1.x,t2.x))
      Node ( t1.x,        // - picked as the root element
             t1.r + 1,    // - the height of the tree is increased by 1
                          // - and the number of nodes is doubled.
             t2 :: t1.c)  // - t1. was picked as the root. Append t2 the bigger one
                          // - as the left most child of of t1

   // Q: Some thing goes here ?
   // make : t2 root the head
    else Node ( t2.x,         // same as above but now pick t2 as the root
                t2.r + 1 ,    // increase the rank.
                t1 :: t2.c )

  /**
   * Insert node t into binomial queue.  preserving the property that
   * the queue is sorted in increasing order of the rank.
   *
   * requires that the rank of tree t less than equal to head of the heap.
   */
  protected def ins(t: Node, ts: H): H = ts match {

      /**
       * if queue is empty then tree t will be
       * the only element in the heap.
       */
      case Nil => List(t)

      // why can we assume this ?
      // t.r <= tp.r
      case tp::ts => // tp - is the (smallest/head) element in the heap.

        /**
         * if the rank of the element to be inserted is less than the
         * rank of head of the heap. then it can safely be inserted
         * before the head element preserving the ascending rank
         * property.
         */
        if ( t.r < tp.r ) t::tp::ts

        /**
         * If the rank is equal to head then we link the head and tree
         * the new node is clearly the smallest ranked element and can
         * be inserted using recursive call to ins.
         */
        else ins(link(t, tp),ts)
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
  override def insert(x: A, ts: H) = ins ( Node(x ,0 ,Nil) , ts)

  /**
   * returns the meld of two queues by matching their ranks
   */
  override def meld(ts1: H, ts2: H) = (ts1, ts2) match {
    // meld of empty list and a full list is simply the other list
    case (Nil, ts) => ts
    case (ts, Nil) => ts
    // the ts1 and ts2 are organized in increazing order of ranks.
    case (t1 :: ts1, t2 :: ts2 ) =>
      // melding proceeds along the head of the list
      // if their ranks are unequal the minimum rank.
      // property of the head of the list is preserved
      if (t1.r < t2.r)      t1 :: meld(ts1,t2::ts2)
      else if (t2.r < t1.r) t2 :: meld(t1::ts1,ts2)
      // oops, the rank of head binomial tree of both lists are equal
      // are equal.
      else ins(link(t1,t2), meld(ts1,ts2))
  }

  /**
   * find the minimum element in the binomial queue. Look through the
   * roots of every tree in the binomial queue. returning the minimum
   * value found. since the size of the binomial queue is limited to
   * O(log(n)). findMin is also going to be O(log(n)).
   */
  override def findMin(ts: H) =
    ts match {
      case Nil =>
        throw new NoSuchElementException("min of empty heap")
      case t::Nil =>
        root(t)
      case t::ts  =>
        val x = findMin(ts)
        // ( root < x )
        if (ord.lteq(root(t),x)) root(t)
        else x
    }

  /**
   * This is the most complicated heap operation:
   *
   * 1. find the tree with the minimum element.
   *
   * 2. delete the minimum element.
   *
   * 3. take the list of child nodes. these child nodes also form a
   *    binomial heap. Go through this child heap and meld it in a manner
   *    analogous to binary addition add this child list to present heap
   *
   * Finding and removing the tree
   *
   */
  override def deleteMin(ts: H) =
    ts match {
      case Nil => throw new NoSuchElementException("delete min of empty heap")
      case t::ts =>
        // t - current minimum tree, ts - rest of the list
        def getMin(t: Node, ts: H) : (Node, H) =
          ts match {
            case Nil     => (t, Nil)                  // the current minimum
                                                      // is the overall minimum
            case tp::tsp =>
              val (tq,tsq) = getMin(tp, tsp)          // current minimum and rest of
                                                      // the tree without the minimum.

              if (ord.lteq(root(t), root(tq))) (t,ts) // if current root is smaller
                                                      // return tail without current root.

              else (tq, t::tsq)                       // return the found minimum,
                                                      // append current head
          }
        val (Node(_,_,c), tsq) = getMin(t, ts)        // return minimum and minimum's children
        meld(c.reverse, tsq)                          // list of children are sorted in ascending order
                                                      // of ranks reverse it and meld with rest of queue.
    }

} // end of heap

trait Bogus1BinomialHeap extends BinomialHeap {
  /**
   * Takes a binomial heap and always returns the root of the first
   * tree with shortest ranked element.
   */
  override def findMin(ts: H) =
    ts match {
      case Nil => throw new NoSuchElementException("min of empty heap")
      case t::ts => root(t)
    }
}

trait Bogus2BinomialHeap extends BinomialHeap {
  /**
   * Instead of linking trees of equal range. Instead
   * of linking the larger tree under the smaller tree
   * we go the other way.
   */
  override protected def link(t1: Node,
                              t2: Node): Node = // t1.r==t2.r
    if (!ord.lteq(t1.x,t2.x)) Node(t1.x, t1.r+1, t2::t1.c)
    else Node(t2.x, t2.r+1, t1::t2.c)
}

trait Bogus3BinomialHeap extends BinomialHeap {
  /**
   * Q: When do we link nodes ?
   * A:
   */
  override protected def link(t1: Node, t2: Node): Node =
    // t1.r==t2.r
    if (ord.lteq(t1.x, t2.x)) // ( t1.x < t2.x )
      Node ( t1.x,
             t1.r + 1,
             t1 :: t1.c)
    else
      Node ( t2.x,
             t2.r + 1,
             t2 :: t2.c)
}

trait Bogus4BinomialHeap extends BinomialHeap {

  /**
   * Breaks [deleteMin] ...
   *
   * Sc: always just deletes the first of t's children.
   * A: But how can deleteMin always get the first child.
   * P: But that would mean that ... deleteMin will not give
   * minimum but the first childd
   * 
   */
  override def deleteMin (ts: H) = ts match {
    case Nil =>
      throw new NoSuchElementException("delete min of empty heap")
    case t::ts => 
      meld(t.c.reverse, ts)
  }
}

trait Bogus5BinomialHeap extends BinomialHeap {
  /**
   * Breaks meld by ???
   */
  override def meld(ts1: H, ts2: H) = ts1 match {
    case Nil => ts2
    case t1::ts1 => List(Node(t1.x, t1.r, ts1 ++ ts2))
  }
}
