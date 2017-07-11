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
    def mergePattern[T](xs : List[T], ys: List[T]) : List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case(x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: mergePattern(xs1, ys)
        else y :: mergePattern(xs, ys1)
    }

    val (first, second) = xs splitAt(n)
    mergePattern(mSort(first)(lt), mSort(second)(lt))
    //merge(mSort(first), mSort(second))
  }
}

val nums = List(1, -4 ,25, 12, -9, 4 ,8)
mSort(nums)((x: Int, y: Int) => x < y)