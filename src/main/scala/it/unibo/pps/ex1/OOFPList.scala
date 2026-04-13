package it.unibo.pps.ex1

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldLeft(Nil())((list, value) => list.append(f(value)))

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)
  
  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] =
    def _zipWithValueRec(value: B, list: List[A]): List[(A, B)] = list match {
      case curr :: rest => (curr, value) :: _zipWithValueRec(value, rest)
      case Nil() => Nil()
    }
    _zipWithValueRec(value, this)
  def zipWithValueWithFold[B](value: B): List[(A, B)] = foldRight(Nil())((curr, acc) => (curr, value) :: acc)

  def length(): Int = foldLeft[Int](0)((acc, _) => acc+1)
  def indices(): List[Int] = foldLeft[(List[Int], Int)](Nil(), this.length()-1){
    case ((list, index), _) => (index :: list, index-1)
  }._1

  def zipWithIndex: List[(A, Int)] =
    def _rec(list: List[A], index: Int): List[(A, Int)] = list match {
      case curr :: rest => (curr, index) :: _rec(rest, index+1)
      case Nil() => Nil()
    }
    _rec(this, 0)
  def zipWithIndexWithFold: List[(A, Int)] = foldRight[(List[(A, Int)], Int)](Nil(), this.length()-1){
    case (curr, (list, index)) => ((curr, index)::list, index-1)
  }._1
  def partition(predicate: A => Boolean): (List[A], List[A]) = {
    def _rec(list: List[A]): (List[A], List[A]) = list match {
      case curr :: rest => if(predicate(curr)){
        val (l1, l2) = _rec(rest)
        (curr :: l1, l2)
      }else{
        val (l1, l2) = _rec(rest)
        (l1, curr :: l2)
      }
      case Nil() => (Nil(), Nil())
    }
    _rec(this)
  }

  def partitionWithFold(predicate: A => Boolean): (List[A], List[A]) = foldRight[(List[A], List[A])](Nil(), Nil()) {
    case (curr, (l1, l2)) => if (predicate(curr)) {
      (curr :: l1, l2)
    } else {
      (l1, curr :: l2)
    }
  }
  def span(predicate: A => Boolean): (List[A], List[A]) =
    def _rec(list:List[A], failure: Boolean): (List[A], List[A]) = list match {
      case curr :: rest => if(!failure && predicate(curr)) {
        val (l1, l2) = _rec(rest, false)
        (curr :: l1, l2)
      }
      else{
        val (l1, l2) = _rec(rest, true)
        (l1, curr :: l2)
      }
      case Nil() => (Nil(), Nil())
    }
    _rec(this, false)

  def spanWithFold(predicate: A => Boolean): (List[A], List[A]) = foldLeft[((List[A], List[A]), Boolean)]((Nil(), Nil()), false){
    case (((l1, l2), failure), curr) => if(!failure && predicate(curr)){
      ((curr :: l1, l2), false)
    }else{
      ((l1,curr :: l2), true)
    }
  }._1

  def takeRight(n: Int): List[A] = {
    def _rec(list:List[A], nLast:Int):List[A] = list match {
      case curr :: rest => if(nLast == n){
        (curr :: rest)
      }else{
        _rec(rest, nLast-1)
      }
      case Nil() => Nil()
    }
    _rec(this, this.length())
  }

  def takeRightWithFold(n: Int): List[A] = foldRight[(List[A], Int)](Nil(), 1){
    case (curr, (l, nLast)) => if(nLast <= n){
      ((curr :: l), nLast+1)
    }
    else{
      ((l), nLast+1)
    }
  }._1

  def collect(predicate: PartialFunction[A, A]): List[A] = this.filter((a) => predicate.isDefinedAt(a)).map((a) => predicate(a))

  def collectWithRec(predicate: PartialFunction[A, A]): List[A] = {
    def _rec(list:List[A]): List[A] = list match {
      case curr :: rest => if(predicate.isDefinedAt(curr)){
        predicate(curr) :: _rec(rest)
      }
      else{
        _rec(rest)
      }
      case Nil() => Nil()
    }
    _rec(this)
  }

// Factories
object List:

  def unzip[A, B](list:List[(A, B)]): (List[A], List[B]) = list match {
    case (left, right) :: rest =>
      val (leftList, rightList) = unzip(rest)
      (left :: leftList, right :: rightList)
    case Nil() => (Nil(), Nil())
  }

  def unzipWithFold[A, B](list:List[(A, B)]): (List[A], List[B]) =
    list.foldRight(Nil(), Nil()){
      case ((left, right), (leftList, rightList)) => (left :: leftList, right :: rightList)
    }

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:
  import List.*
  val reference = List(1, 2, 3, 4)
  println(unzip(List((1,2),(4,3))))
  println(unzipWithFold(List((1,2),(4,3))))
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.zipWithValueWithFold(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.length()) // 4
  println(reference.indices()) // List(0, 1, 2, 3)
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.zipWithIndexWithFold) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.partitionWithFold(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.spanWithFold(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.spanWithFold(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.takeRightWithFold(3)) // List(2, 3, 4)
  println(reference.collectWithRec { case x if x % 2 == 0 => x + 1 }) // List(3, 5)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)