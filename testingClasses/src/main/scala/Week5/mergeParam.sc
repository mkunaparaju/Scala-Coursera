import math.Ordering

def mSort[T](xs: List[T])(lt : (T,T) => Boolean) : List[T] = {
  val n = xs.length/2
  if(n == 0) xs
  else {

//    def merge(a : List[Int], b: List[Int]) : List[Int] = a match {
//      case Nil => b
//      case x :: xs1 => b match {
//        case Nil => a
//        case y :: ys1 => {
//          if (x < y) x :: merge(xs1, b)
//          else y :: merge(a, ys1)
//        }
//      }
//    }
    // using pattern matching with tuples
    def mergePattern(xs : List[T], ys: List[T]) : List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case(x :: xs1, y :: ys1) =>
        if (lt( x, y)) x :: mergePattern(xs1, ys)
        else y :: mergePattern(xs, ys1)
    }

    val (first, second) = xs splitAt(n)
    mergePattern(mSort(first)(lt), mSort(second)(lt))
    //merge(mSort(first), mSort(second))
  }
}

def mSortOrdering[T](xs: List[T])(implicit ord: Ordering[T]) : List[T] = {
  val n = xs.length/2
  if(n == 0) xs
  else {

    //    def merge(a : List[Int], b: List[Int]) : List[Int] = a match {
    //      case Nil => b
    //      case x :: xs1 => b match {
    //        case Nil => a
    //        case y :: ys1 => {
    //          if (x < y) x :: merge(xs1, b)
    //          else y :: merge(a, ys1)
    //        }
    //      }
    //    }
    // using pattern matching with tuples
    def mergePattern(xs : List[T], ys: List[T]) : List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case(x :: xs1, y :: ys1) =>
        if (ord.lt( x, y)) x :: mergePattern(xs1, ys)
        else y :: mergePattern(xs, ys1)
    }

    val (first, second) = xs splitAt(n)
    mergePattern(mSortOrdering(first), mSortOrdering(second))
    //merge(mSort(first), mSort(second))
  }
}

val nums = List(1, -4 ,25, 12, -9, 4 ,8)
mSort(nums)((x: Int, y: Int) => x < y)

val fruits = List("apple", "bana", "pi", "oran")
// can be used for nums as well
mSort(fruits)((x, y) => x.compareTo(y) < 0)

mSortOrdering(nums)
mSortOrdering(fruits)

