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
  
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimum. ") =
    forAll { (h: H) =>
      
      def asSortedList_r(acc:List[Int],h:H):List[Int] = h match {
        case heap if heap == empty  =>  acc
        case _ => 
          val m = findMin(h)
          asSortedList_r(m::acc,deleteMin(h))
      }

      def asSortedList(h:H):List[Int] = asSortedList_r(List[Int](),h).reverse

      val heapData = asSortedList(h)
      // Check sorted in cleaner way
      var sorted = true
      var prev = findMin(h)

      for (v <- heapData) {
        if(v < prev) 
          sorted = false
        prev = v
      }

      sorted
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = 
    forAll { (h1: H,h2:H) =>

      val h = meld(h1,h2)
      val m1 = findMin(h1)
      val m2 = findMin(h2)      
      
      findMin(h) == math.min(m1,m2)
    }

  // TODO Still Breaking 

  // Bogus (3) *** FAILED *** -> [Takes wrong child in link]
  
  // Bogus (4) *** FAILED *** -> [Something Here]
  

}
