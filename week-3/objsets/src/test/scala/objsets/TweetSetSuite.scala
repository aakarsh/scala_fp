package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {

    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))

    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)

    val set4c = set3.incl(c) // a,b,c
    val set4d = set3.incl(d) // a,b,d
    val set5  = set4c.incl(d) // a,b,c,d

  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      println("4c size : "+set4c.size)
      println("4d size : "+set4d.size)
      println("4d+4c size : "+(set4c.union(set4d)).size)
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("heavy load") {
    val n = 100000
    var foo  = new Array[Tweet](n)
    for(i <- 0 to n-1) {
      foo(i) = new Tweet("foo-twat","foo:tweet---"+i,i)
    }

    var bar  = new Array[Tweet](n)
    for(i <- 0 to n-1) {
      bar(i) = new Tweet("bar-twat","bar:tweet---"+i,i)
    }

    var ts:TweetSet = new Empty
    for(tweet <- foo) {
      ts = ts.incl(tweet)
    }

    var ts2:TweetSet = new Empty
    for(tweet <- bar) {
      ts2 = ts2.incl(tweet)
    }

    ts.union(ts2)
        
  }
  
}
