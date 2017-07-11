def Last[T](xs: List[T]) : T = xs match {
  case List() => throw new Error("empty list")
  case List(x) => x
  case y :: ys => Last(ys)
}

def init[T](xs: List[T]) : List[T] = xs match {
  case List() => throw new Error("empty list")
  case List(x) => List()
  case y :: ys  => y :: init(ys)
}


def concat[T](xs: List[T], ys: List[T]) : List[T] = xs match {
  case List() => ys
  case z :: zs  => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]) : List[T] = xs match {
  case List() => xs
  case y :: ys  => reverse(ys) ++ List(y)
}

def removeAt[T](n : Int, xs: List[T]) : List[T] =
  (xs take n)  ::: (xs drop n+1)

def mSort (xs: List[Int]) : List[Int] = {
  val n = xs.length/2
  if(n == 0) xs
  else {

    def merge(a : List[Int], b: List[Int]) : List[Int] = a match {
      case Nil => b
      case x :: xs1 => b match {
        case Nil => a
        case y :: ys1 => {
          if (x < y) x :: merge(xs1, b)
          else y :: merge(a, ys1)
        }
      }
    }
    // using pattern matching with tuples
    def mergePattern(xs : List[Int], ys: List[Int]) : List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case(x :: xs1, y :: ys1) =>
        if (x < y) x :: mergePattern(xs1, ys)
        else y :: mergePattern(xs, ys1)
    }

    val (first, second) = xs splitAt(n)
    mergePattern(mSort(first), mSort(second))
    //merge(mSort(first), mSort(second))
  }
}

val pair = ("ans", 33)
val (label,value) = pair
val nums = List(1, -4 ,25, 12, -9, 4 ,8)
mSort(nums)



