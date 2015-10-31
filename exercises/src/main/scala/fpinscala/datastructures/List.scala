package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  //@annotation.tailrec
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("empty list does not have head")
      case Cons(_,t) => Cons(h,t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    n match {
      case x if x < 0 => sys.error("n is minus value")
      case 0 => l
      case _ => l match {
        case Nil => sys.error("drop head on empty list")
        case Cons(_,t) => drop(t, n-1)
      }
    }
  // 예외를 안던지는 버젼으로 다시
  def dropS[A](l: List[A], n: Int): List[A] =
    if (n<=0) Nil
    else l match {
      case Nil => Nil
      case Cons(_,t) => dropS(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs, f) else l
    }
  // 답 한 번 보고 다시. 이게 더 직관적이네
  def dropWhileR[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhileR(t, f)
      case _ => l // Nil 도 포함
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h,t) if (t != Nil) => Cons(h,init(t))
      case _ => Nil
    }
  def initWithException[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error ("init of empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, initWithException(t))
  }

  /*
  Example 3.8 : List(1,2,3)을 그대로 리턴하지만 마지막까지 훑는데 n=3만큼의 시간이 필요하다.
   */

  // Example 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,n) => 1+n)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

object TestList {
  import List._

  def main(args: Array[String]): Unit = {
    val input = List(1,2,3)
    val result = foldRight(input, Nil:List[Int])(Cons(_,_))
    println(result)
    println(input.hashCode())
    println(result.hashCode())
  }
}