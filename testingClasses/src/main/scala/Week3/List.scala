package Week3
import java.util.NoSuchElementException

/**
  * Created by mkunaparaju on 6/10/2017.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false

}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  //exception type is nothing
  //nothing is a subtype of any type
  def head: Nothing = throw new NoSuchElementException("Nill.head")
  def tail: Nothing = throw new NoSuchElementException("Nill.tail")
}

object List {
  // List(1,2)
  //used in function position
  // translates to: List.apply(1,2)

  def apply[T](x1: T, x2: T) : List[T] = new Cons(x1, new Cons[T](x2, new Nil[T]))
  def apply[T](): List[T] = new Nil[T]
}