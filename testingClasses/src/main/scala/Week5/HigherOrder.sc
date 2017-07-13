//multiply ech element of  list by same factor
def scaleList(xs: List[Double], factor: Double) : List[Double] = xs match {
  case Nil => xs
  case y::ys =>   (y*factor) :: scaleList(ys, factor)
}

////generalized map function
//abstract class List[T] {
//  def map[U](f: T => U) : List[U] = this match {
//    case Nil => this
//    case x::xs => f(x) :: xs.map(f)
//}
//
//}

//with map
//example of transformation
def ScalelistMap(xs: List[Double], factor: Double): List[Double] = xs map(x => x*factor)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => (y*y) :: squareList(ys)
}

def squareListMap(xs: List[Int]): List[Int] = xs map (x => x*x)

//example of filtering
//select all positive elements
def posElem(xs: List[Int]) : List[Int] = xs match {
  case Nil => xs
  case y::ys => if(y>0) y::posElem(ys) else posElem(ys)
}

def posElemMap(xs: List[Int]): List[Int] = xs filter(x => x > 0)

val nums = List(31, -4 ,25, 12, -9, 4 ,8)
val fruits = List("apple", "bana", "pi", "oran")

nums filter(x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)

nums takeWhile(x => x > 0)
nums dropWhile(x => x > 0)
nums span(x => x > 0)


//write a funtion pack that packs duplicate consecutives into sub lists
def pack[T](xs: List[T]) : List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys =>
    val(f, r) = xs span(p => (p == y))
    f :: pack(r)
}
pack(nums)

def encode[T](xs: List[T]): List[(T, Int)] = {

  pack(xs) map (ys => (ys.head, ys.length))


}
