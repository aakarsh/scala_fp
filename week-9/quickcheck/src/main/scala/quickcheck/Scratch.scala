import quickcheck._
import org.scalacheck._

// Scratch buffer for excercising the code
object Scratch {

  /**
   * Create a heap and delete elements from it.
   */
  def testHeapCreation(n:Int) = {
    object HeapBuilder extends BinomialHeap with IntHeap
    //
    import HeapBuilder._
    //
    println("reloading file ... ")
    // start with empty heap.
    var sh: List[HeapBuilder.Node] = empty
    // insert n elements.
    for (i <- 0 to n)
        sh = insert(i, sh)
    // print the heap
    println(sh)
    // delete minimum and find minimum
    for(i <- 0 to n) {
      println("minimum : " + HeapBuilder.findMin(sh))
      sh = HeapBuilder.deleteMin(sh)
    }
  }

  /**
   * Test generate random heap
   */
  def testGenerateRandomHeap(n: Int) = {

    object HeapBuilder extends BinomialHeap with IntHeap

    import HeapBuilder._
    //
    val minValue:Int = 0
    val maxValue:Int = n
    // need to take single-heap and thread it to join them


    def heapGen: Gen[HeapBuilder.H] =
      for {
        n    <- Gen.choose(minValue,maxValue)
        rest <- Gen.option(heapGen)
      } yield insert(n,rest.getOrElse(empty))





    val g_heap  =
      Gen.choose(minValue, maxValue)
         .map   { (number:Int) => insert(number, empty) }

    println("g_heap : " + g_heap )

    //-*-:-something-here
    println("--------------------------------------------------")
    //
    for(i <- 0 to n)
      println(heapGen.sample)
    //
    println("--------------------------------------------------")
  }

  def testScalaCheckDiscoveredFailures() =  {

    object HeapBuilder extends BinomialHeap with IntHeap
    import HeapBuilder._

    // x is a heap
    val x = insert(99,insert(88,insert(95,empty)))
    println(sortedList(x))
    // so we are keeping it sorted
    val y = meld(x,x)
    println(sortedList(y))
  }

  /**
   * Create a heap of 100 elements and delete them
   */
  def run() = {
    testHeapCreation(10)
    testGenerateRandomHeap(5)
    testScalaCheckDiscoveredFailures()
  }
}

//Scratch.run()
