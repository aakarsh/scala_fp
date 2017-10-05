package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


/**
 * Extend from Int heap include various properties of heaps.
 */
abstract class QuickCheckHeap extends Properties ("Heap")
                              with IntHeap {

  val minValue = 0
  val maxValue = 100

  /**
   * Generate a heap which is or arbitrary length between
   * minValue and maxValue.
   */
  lazy val genHeap : Gen[H] =
    for {
        n    <- Gen.choose(minValue,maxValue)
        rest <- Gen.option(genHeap)
    } yield insert(n,rest.getOrElse(empty))

  // Arbitrary Instance of: genHeap -- picked up implicitly
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("inserting minimum into heap returns minimum") =
    forAll { (h: H) =>
      // current minimum
      val curMin = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(curMin, h)) == curMin
  }

  /**
   * Basic heap properties.
   */
  property("single element heap returns that element back.") =
    forAll {a: A =>
      val h = insert(a, empty)
      findMin(h) == a
    }

  property("two element heap returns that minimum back.") =
    forAll {(a: A,b:A) =>
      val m = math.min(a,b)
      val h = insert(b,insert(a, empty))
      findMin(h) == m
    }

  property("inserting and element into heap then deleting leads to empty heap.") =
    forAll { (a: A) =>
      val h = insert(a, empty)
      val h2 = deleteMin(h)
      h2 == empty
    }

  // check sorted in cleaner way
  def isSorted(heap:H) : Boolean ={
    var prev   = findMin(heap)
    var sorted = true
    for (v <- sortedList(heap)) {
      if(v < prev) sorted = false
      prev = v
    }
    sorted
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimum. ") =
    forAll { (h: H) => isSorted(h) }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (h1: H, h2:H) =>

      val h  = meld(h1,h2)
      val m1 = findMin(h1)
      val m2 = findMin(h2)

      findMin(h) == math.min(m1,m2)
    }

  property("Creating a heap from a random list will produce a sorted list when needed.") =
    forAll { ( ls:List[Int] ) =>
      // build the heap
      var heap = empty
      for(l <- ls) {
        heap = insert(l,heap)
      }
      sortedList(heap) == ls.sorted
      
    }
}
