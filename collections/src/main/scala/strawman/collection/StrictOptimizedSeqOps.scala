package strawman.collection

import scala.{Int, math, Any}

/**
  * Trait that overrides operations on sequences in order
  * to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps [+A, +CC[_], +C]
  extends Any
    with SeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def distinctBy[B](f: A => B): C = {
    val isImmutable = this.isInstanceOf[immutable.Seq[_]]
    if (isImmutable && lengthCompare(1) <= 0) coll
    else {
      val builder = newSpecificBuilder()
      val seen = mutable.HashSet.empty[B]
      var it = this.iterator()
      var different = false
      while (it.hasNext) {
        val next = it.next()
        if (seen.add(f(next))) builder += next else different = true
      }
      if (different) builder.result() else coll
    }
  }

  override def prepended[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b += elem
    b ++= this
    b.result()
  }

  override def appended[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b ++= this
    b += elem
    b.result()
  }

  override def appendedAll[B >: A](suffix: Iterable[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    b ++= this
    b ++= suffix
    b.result()
  }

  override def prependedAll[B >: A](prefix: Iterable[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    b ++= prefix
    b ++= this
    b.result()
  }

  override def padTo[B >: A](len: Int, elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    val L = size
    b.sizeHint(math.max(L, len))
    var diff = len - L
    b ++= this
    while (diff > 0) {
      b += elem
      diff -= 1
    }
    b.result()
  }

}
