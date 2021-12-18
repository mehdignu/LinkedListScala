package com.rockthejvm.test

import scala.annotation.tailrec
import scala.annotation.tailrec

abstract class LList[A] {
  def head: A

  def tail: LList[A]

  def isEmpty: Boolean

  def add(elem: A): LList[A] = Cons(elem, this)

  // concatenation
  infix def ++(anotherList: LList[A]): LList[A]

  def map[B](transformer: A => B): LList[B]

  def filter(predicate: A => Boolean): LList[A]

  def flatmap[B](transformer: A => LList[B]): LList[B]

  def foreach(func: A => Unit): Unit

  def sort(compare: (A, A) => Int): LList[A]

  def zipWith[B, T](list: LList[T], zip: (A, T) => B): LList[B]

  def foldLeft[B](start: B)(operator: (B, A) => B): B
}

//trait Predicate[T] {
//  def test(elem: T): Boolean
//}
//
//
//trait Transformer[A, B] {
//  def transform(value: A): B
//}
//
//class DoublerList extends Transformer[Int, LList[Int]] {
//  override def transform(value: Int): LList[Int] =
//    Cons(value, Cons(value + 1, new Empty))
//}

case class Empty[A]() extends LList[A] {
  override def head: A = throw new NoSuchElementException

  override def tail: LList[A] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override infix def ++(anotherList: LList[A]): LList[A] = anotherList

  override def map[B](transformer: A => B): LList[B] = Empty()

  override def filter(predicate: A => Boolean): LList[A] = this

  override def flatmap[B](transformer: A => LList[B]): LList[B] = Empty()

  override def foreach(func: A => Unit): Unit = ()

  override def sort(compare: (A, A) => Int): LList[A] = this

  override def zipWith[B, T](list: LList[T], zip: (A, T) => B): LList[B] =
    if (!list.isEmpty) throw new IllegalArgumentException("Empty list zipping with non-empty list")
    else Empty()

  override def foldLeft[B](start: B)(operator: (B, A) => B): B = start
}

case class Cons[A](override val head: A, override val tail: LList[A]) extends LList[A] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def concatenateElements(remainder: LList[A], acc: String): String =
      if (remainder.isEmpty) acc
      else concatenateElements(remainder.tail, s"$acc, ${remainder.head}")

    s"[${concatenateElements(this.tail, s"$head")}]"
  }

  override def map[B](transformer: A => B): LList[B] =
    Cons(transformer(head), tail.map(transformer))

  override def filter(predicate: A => Boolean): LList[A] =
    if (predicate(head)) Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  override infix def ++(anotherList: LList[A]): LList[A] =
    Cons(head, tail ++ anotherList)

  override def flatmap[B](transformer: A => LList[B]): LList[B] =
    transformer(head) ++ tail.flatmap(transformer)

  override def foreach(func: A => Unit): Unit = {
    func(head)
    tail.foreach(func)
  }

  override def sort(compare: (A, A) => Int): LList[A] = {
    // insertion sort, O(n²), stack recursive
    def insert(elem: A, sortedList: LList[A]): LList[A] =
      if (sortedList.isEmpty) Cons(elem, Empty())
      else if (compare(elem, sortedList.head) <= 0) Cons(elem, sortedList)
      else Cons(sortedList.head, insert(elem, sortedList.tail))

    val sortedTail = tail.sort(compare) // assumed to be a sorted list
    insert(head, sortedTail) // insert head into sorted tail
  }



  override def foldLeft[B](start: B)(operator: (B, A) => B): B =
    tail.foldLeft(operator(start, head))(operator)

  override def zipWith[B, T](list: LList[T], zip: (A, T) => B) =
    if (list.isEmpty) throw new IllegalArgumentException("Zipping list of non equal length")
    else Cons(zip(head, list.head), tail.zipWith(list.tail, zip))
}

object LList {
  def find[A](list: LList[A], predicate: A => Boolean): A =
    if (list.isEmpty) throw new NoSuchElementException
    else if (predicate(list.head)) list.head
    else find(list.tail, predicate)
}

object LListTest {

  def main(args: Array[String]): Unit = {
    val empty = Empty[Int]()
    val first = Cons[Int](1, Cons[Int](2, empty))
//    println(first)

    //    val doubler = new Function1[Int, Int] {
    //      override def apply(value: Int): Int = value * 2
    //    }
    //
    //
    //    val doublerList = new Function[Int, LList[Int]] {
    //      override def apply(value: Int): LList[Int] =
    //        Cons(value, Cons(value + 1, new Empty))
    //    }
    //
    //    val EvenPredicate = new Function1[Int, Boolean] {
    //      override def apply(elem: Int): Boolean = elem % 2 == 0
    //    }

    // map testing
    val numbersDoubled = first.map(_ * 2)
//    println(numbersDoubled)
    val numbersNested = first.map(value => Cons(value, Cons(value + 1, new Empty)))
//    println(numbersNested)

    // filter testing
    val first3Numbers = Cons(1, Cons(2, Cons(3, empty)))


    val onlyEvenNumbers = first3Numbers.filter(_ % 2 == 0)
//    println(onlyEvenNumbers)

    //test concat
    val firstttt = Cons(10, Cons(25, Cons(35, empty)))

    val listConcated = first3Numbers ++ firstttt
//    println(listConcated)


    // test flatmpam
    val flattened = first3Numbers.flatmap(value => Cons(value, Cons(value + 1, new Empty)))
//    println(flattened)

//    println(LList.find[Int](first3Numbers, _ % 2 == 0))
    //    println(LList.find[Int](first3Numbers, new Predicate[Int] {
    //      override def test(elem: Int): Boolean = elem > 5
    //    }))
    val ss = Cons(10, Cons(5, Cons(30, empty)))

    println(ss.sort((x, y) => x - y))
    println(ss.foldLeft(0)((x, y) => x - y))

  }

}
