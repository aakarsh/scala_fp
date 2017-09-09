package objsets

import java.util.NoSuchElementException
import scala.reflect.ClassTag
import scala.reflect.Manifest
import TweetReader._

object Common {


  /**
   * Insead of recursive merge we may need non-recursive merge
   * // TODO : Needs to be tail recursive 
   */
  def merge_r[T](l1: List[T], l2: List[T], compare:(T,T) => Boolean ): List[T] = {
    if (l1.isEmpty)      { l2 }
    else if (l2.isEmpty) { l1 }
    else {
      val h1 = l1.head
      val h2 = l2.head

      if(compare(h1,h2))
        h1::merge_r(l1.tail,l2,compare)
      else
        h2::merge_r(l1,l2.tail,compare)
    }
  }

  def dedup_r[T](prev_head:T, t:List[T], acc:List[T]) : List[T] = {
    if(t.isEmpty) acc
    else if (prev_head == t.head) {
      dedup_r(prev_head, t.tail, acc)
    } else // append head
      dedup_r(t.head,t.tail,t.head::acc)
  }

  def dedup[T](t:List[T]) : List[T] = {
    if(t.isEmpty) List[T]()
    else  
      dedup_r(t.head, t.tail, t.head::List[T]()).reverse
  }
  
}

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {

  override def equals(other: Any) : Boolean = 
    other match {
      case that: Tweet =>  {
        (user == that.user) && (text == that.text) && (retweets == that.retweets)
      }
      case _ => false
    }
  
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a
 * binary search tree. Every branch in the tree has two children (two
 * `TweetSet`s). There is an invariant which always holds: for every
 * branch `b`, all elements in the left subtree are smaller than the
 * tweet at `b`. The elements in the right subtree are larger.
 * Note that the above structure requires us to be able to compare two
 * tweets (we need to be able to say which of two tweets is larger, or
 * if they are equal). In this implementation, the equality/order of
 * tweets is based on the tweet's text (see `def incl`). Hence, a
 * `TweetSet` could not contain two tweets with the same text from
 * different users.
 *
 * The advantage of representing sets as binary search trees is that
 * the elements of the set can be found quickly. If you want to learn
 * more you can take a look at the Wikipedia page [1], but this is not
 * necessary in order to solve this assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the
   * elements in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain
   * abstract and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p,new Empty)

  /**
   * This is a helper method for `filter` that propagetes the
   * accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   *
   */
  def union(that: TweetSet): TweetSet


  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet


  /**
   * Returns a list containing all tweets of this set, sorted by
   * retweet count in descending order. In other words, the head of
   * the resulting list should have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it
   * remain abstract and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = {

    val tweet = this.mostRetweeted
    val smaller = this.remove(tweet)
    if (smaller.isEmpty)
      Nil
    else
      // TODO: Fix stackoverflow here
      new Cons(tweet,smaller.descendingByRetweet)
  }

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   *
   */
  def foreach(f: Tweet => Unit): Unit

  def isEmpty: Boolean

  def size: Int;

  /**
   * Create a sorted list by doing a lvr ordering of the list.
   */
  def toList(): List[Tweet]

  def toListAcc(acc:List[Tweet]) : List[Tweet]

}

class Empty extends TweetSet {

  override def isEmpty: Boolean = true

  override def union(that: TweetSet): TweetSet = that

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc


  def size: Int = 0

  /**
   * Empty sets have no retweets.
   */
   override def mostRetweeted: Tweet =
     throw new NoSuchElementException()

  /**
   * The following methods are already implemented
   */
  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def toList(): List[Tweet] = List[Tweet]()

  override def toListAcc(acc:List[Tweet]): List[Tweet] = acc

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  override def isEmpty: Boolean = false

  def makeLeaf(elem: Tweet): TweetSet =
    new NonEmpty(elem , new Empty , new Empty )

  def merge(l1:List[Tweet], l2:List[Tweet], compare:(Tweet,Tweet) => Boolean)  : List[Tweet]  = {

    var a1 =  l1.toArray
    var a2 =  l2.toArray

    val n = l1.size + l2.size

    var merged = new Array[Tweet](n)
    var p1 = 0
    var p2 = 0
    var i  = 0

    while(i < Math.min(p1,p2)) {
      var h1 = a1(p1)
      var h2 = a2(p2)

      if(compare(h1,h2)) {
        merged(i) = h1
        p1 += 1
        i+=1
      } else{
        merged(i) = h2
        p2 += 1
        i+=1
      }

    }

    while(p1 < a1.size) {
      merged(i) = a1(p1)
      i+=1
      p1+=1
    }

    while(p2 < a2.size) {
      merged(i) = a2(p2)
      i+=1
      p2+=1
    }
    merged.toList
  }


  // TODO: Need to reduce excessive memory consumption here
  override def union(that: TweetSet): TweetSet = {

    def buildTree_r(start:Int, end:Int,
                    sorted: Array[Tweet], depth: Int ) : TweetSet = {

      if( end < start || end < 0 || start > (sorted.length-1) ) {
        new Empty
      } else if ( start == end ) {
        makeLeaf(sorted(start))
      } else if (Math.abs( start - end ) == 1 )  {
        val s = makeLeaf(sorted(start))
        new NonEmpty(sorted(end),s ,new Empty)
      } else {
        val middle = (start + end) / 2
        val left  = buildTree_r(start       ,   middle - 1,    sorted, depth+1)
        val right = buildTree_r(middle + 1, end     ,      sorted, depth+1)
        new NonEmpty(sorted(middle),left,right)
      }
    }

    def buildTree(sorted:Array[Tweet]): TweetSet =
      buildTree_r(0, sorted.length - 1 ,sorted,0)

    def cmp(a:Tweet,b:Tweet):Boolean = a.text <= b.text

    import Common._

    val mergedArray = dedup(merge(this.toList, that.toList, cmp)).toArray

    buildTree(mergedArray)

  }

  override def toListAcc(acc:List[Tweet]): List[Tweet] = {
    val leftList  = this.left.toListAcc(this.elem :: acc)
    val finalList = this.right.toListAcc(leftList)
    finalList
  }

  def toList(): List[Tweet] = {
    val l  = this.toListAcc(List[Tweet]())
    l.reverse
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val l = left.filterAcc(p,acc)
    val v = if (p(elem)) l.incl(elem) else l
    right.filterAcc(p,v)
  }

  /**
   *
   */
   override def mostRetweeted: Tweet = {

     def optional(e: TweetSet): Option[Tweet] = {
       if(e.isEmpty) None
       else Some(e.mostRetweeted)
     }

     def max(l:Option[Tweet],r:Option[Tweet]): Option[Tweet] = {
       l match {
         case None => r
         case Some(le) => {
           r match {
             case None => l
             case Some(re) => if (le.retweets > re.retweets) l else r
           }
         }
       }
     }

     val lt = optional(this.left)
     val rt = optional(this.right)

     max(max(lt,rt),Some(elem)).getOrElse(elem)
   }



  /**
   * The following methods are already implemented
   */
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text)
      new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text)
           new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  /**
   * Performs a vlr traversal of the tree.
   */
  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def size: Int = this.left.size + 1  + this.right.size

}

trait TweetList {

  def head: Tweet
  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  def toList() : List[Tweet];

  def reverse(): TweetList;

}

object Nil extends TweetList {

  def head =
    throw new NoSuchElementException("head of EmptyList")

  def tail =
      throw new NoSuchElementException("tail of EmptyList")

  override def reverse(): TweetList = this

  override def toList() : List[Tweet] = List[Tweet]()



  def isEmpty = true

}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {

  def isEmpty = false

  override def toList() : List[Tweet] = {
    if(tail.isEmpty)
      List[Tweet](head)
    else
      head::(tail.toList())
  }

  override def reverse(): TweetList = {
    val r = this.toList().reverse

    def fromList(ls : List[Tweet]): TweetList = {
      if(ls.isEmpty) Nil
      else new Cons(ls.head,objsets.Nil)
    }

    fromList(r)
  }

}


object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple  = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def text_contains(text:String, keywords: List[String]): Boolean =
    if(keywords.isEmpty) false
    else if(text.contains(keywords.head)) true
    else text_contains(text,keywords.tail)



  lazy val googleTweets: TweetSet =
    TweetReader.allTweets.filter((tweet:Tweet) => text_contains(tweet.text,google))


  lazy val appleTweets: TweetSet =
    TweetReader.allTweets.filter((tweet:Tweet) => text_contains(tweet.text,apple))


  /**
   * A list of all tweets mentioning a keyword from either apple or
   * google, sorted by the number of retweets.
   */

  lazy val trending: TweetList =
    // lots of wondeful out of memory exceptions on 8Gb machine
    googleTweets.union(appleTweets).descendingByRetweet

}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
