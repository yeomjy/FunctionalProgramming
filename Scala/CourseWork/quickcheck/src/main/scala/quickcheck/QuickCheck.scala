package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

//For doing this, you can take inspiration from the lecture on generators and monads. Here are some basic generators that you can combine together to create larger ones:
//
//    arbitrary[T] is a generator that generates an arbitrary value of type T. As we are interested in IntHeaps it will generate arbitrary integer values, uniformly at random.
//
//    oneOf(gen1, gen2) is a generator that picks one of gen1 or gen2, uniformly at random.
//
//    const(v) is a generator that will always return the value v.
//
//    You can find many more useful ones either in the ScalaCheck user guide or in the Scaladocs.

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = {
    for {
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  }
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert2") = {
      val h: H = empty
      val x = arbitrary[A].sample.get
      val y = arbitrary[A].sample.get
      val min = if x < y then x else y
      findMin(insert(y, insert(x, h))) == min
  }
    //          val (x, y): (Int, Int) = for (x <- arbitrary[Int]; y <- arbitrary[Int]) yield (x, y)

  property("insertDelete") =  {
      val h: H = empty
      val x = arbitrary[A].sample.get
      isEmpty(deleteMin(insert(x, h)))
  }

  property("sorted") = forAll { (h: H) => {
      def removeIter(h: H): List[A] = {
          if isEmpty(h) then Nil
          else findMin(h) :: removeIter(deleteMin(h))
      }
      val l = removeIter(h)
      l == l.sorted
//      removeIter(h).sorted
    }
  }

  property("meld") = forAll { (h1: H, h2: H) => {
      val m1 = if isEmpty(h1) then 0 else findMin(h1)
      val m2 = if isEmpty(h2) then 0 else findMin(h2)
      val h = meld(h1, h2)
      val m = if isEmpty(h) then 0 else findMin(h)
//      val mm = if m1 < m2 then m1 else m2
      val mm = if isEmpty(h1) then (if isEmpty(h2) then 0 else m2) else (if isEmpty(h2) then m1 else (if m1 < m2 then m1 else m2))
      m == mm
  }
  }

  property("meldAndSorted") = forAll { (h1:H, h2:H) => {
      def removeIter(h: H): List[A] = {
          if isEmpty(h) then Nil
          else findMin(h) :: removeIter(deleteMin(h))
      }
      val h = meld(h1, h2)
      val l = removeIter(h)
      val l1 = removeIter(h1)
      val l2 = removeIter(h2)
      (l == l.sorted) && (l == (l1 ++ l2).sorted)

  }

  }

  property("insertDelete2") = forAll {(h: H) =>
      val m = if isEmpty(h) then 0 else findMin(h)
      findMin(insert(m, deleteMin(h))) == m
  }

  property("insertDelete3") = forAll { (h: H) =>
      val m = if isEmpty(h) then 0 else findMin(h)
      val n = arbitrary[A].sample.get
      val hh = insert(n, h)
      val mm = findMin(hh)
      mm == m || mm == n
  }

  property("SomeInsert") = {
      val h = empty
      val h1 = insert(-100, h)
      val h2 = insert(-99, h1)
      val h3 = insert(-100, h2)
      val h4 = insert(-100, h3)
      val h5 = insert(-101, h4)
      val h6 = insert(1, h5)
      def removeIter(h: H): List[A] = {
          if isEmpty(h) then Nil
          else findMin(h) :: removeIter(deleteMin(h))
      }
      val l = removeIter(h6)
      l == l.sorted
  }

