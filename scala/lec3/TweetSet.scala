package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet 

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = {
    var ret: TweetSet = this;
    def f(p: Tweet): Unit = {
      if (!this.contains(p)) ret = ret.incl(p)
    }
    that.foreach(f)
    ret
  }

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
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = {
    
    def recusive(tl: TweetList, t: Tweet): TweetList = {
      if (tl.isEmpty) new Cons(t, Nil)
      else if (tl.tail.isEmpty) new Cons(tl.head, new Cons(t, Nil))
      else new Cons(tl.head, recusive(tl.tail, t))
    }
    
    var rem: TweetSet = this
    var list: TweetList = Nil
    
    while (!rem.isInstanceOf[Empty]) {
      var t: Tweet = rem.mostRetweeted
      list = recusive(list, t)
      rem = rem.remove(t)
    }
    
    list
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
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  
  def filter(p: Tweet => Boolean): TweetSet = new Empty

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc


  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  def mostRetweeted: Tweet = throw new Exception("NonSuchElement")
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  
  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p: Tweet => Boolean, new Empty)
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    var ret: TweetSet = acc
    def f(t: Tweet): Unit = {
      if (p(t)) ret = ret.incl(t)
    }
    foreach(f)
    ret
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
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  
  def mostRetweeted: Tweet = {
    var ret: Tweet = this.elem
    if (! this.left.isInstanceOf[Empty]) {
      var tmp: Tweet = this.left.mostRetweeted
      if (ret.retweets < tmp.retweets) ret = tmp
    }
    
    if (! this.right.isInstanceOf[Empty]) {
      var tmp: Tweet = this.right.mostRetweeted
      if (ret.retweets < tmp.retweets) ret = tmp
    }
    ret
  }
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
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")


  lazy val googleTweets: TweetSet = {
    var tmp: TweetSet = new Empty
    def f(t: Tweet): Unit = {
      if (google.exists(str => t.text.contains(str))) tmp = tmp.incl(t)
    }
    TweetReader.allTweets.foreach(f)
    tmp
  }
  lazy val appleTweets: TweetSet = {
    var tmp: TweetSet = new Empty
    def f(t: Tweet): Unit = {
      if (apple.exists(str => t.text.contains(str))) tmp = tmp.incl(t)
    }
    TweetReader.allTweets.foreach(f)
    tmp
  }

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = {
    appleTweets.union(googleTweets).descendingByRetweet
  }
}

object Main extends App {
  println("hello")
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
