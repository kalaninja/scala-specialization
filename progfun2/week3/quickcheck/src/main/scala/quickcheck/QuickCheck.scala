package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("test1") = forAll { (a: Int, b: Int) =>
    val m = a min b
    val h = insert(a, insert(b, empty))
    findMin(h) == m
  }

  property("test2") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("test3") = forAll { h: H =>
    def check(h: H, prev: Int): Boolean = {
      if (isEmpty(h)) true
      else {
        val curr = findMin(h)
        curr >= prev && check(deleteMin(h), curr)
      }
    }

    check(h, Int.MinValue)
  }

  property("test4") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val m = findMin(h)

    m == findMin(h1) || m == findMin(h2)
  }

  property("test5") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("test6") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = m1 min m2
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }
}
