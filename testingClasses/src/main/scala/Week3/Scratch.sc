import Week3._

new Rational(1,2)

def error(msg: String) = throw new Error(msg)

//error("testttt")

val x = null

val y: String = x

//val z: Int = null -- not applicable

def nth[T](index: Int, list: List[T]): T = {
  if (index == 0) list.head
  else if ((index > 0 && list.isEmpty) || index < 0) throw new IndexOutOfBoundsException
  else nth(index - 1, list.tail)
}
val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(2, list)
nth(4, list)





