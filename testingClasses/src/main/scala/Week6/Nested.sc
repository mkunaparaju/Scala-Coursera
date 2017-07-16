val n = 7

//((1 until n) map(i => (1 until i) map (j => (i, j)))).flatten
//simpler way
((1 until n) flatMap(i => (1 until i) map (j => (i, j)))).filter(pair => isPrime(pair._1 + pair._2))


def isPrime(n : Int) : Boolean = (2 until n) forall(x => (n%x) != 0)

//level of abstraction above
//head hurts
//can use for keyword
//for (p <- persons if p.age > 20) yield p.nam7
//above same as :: persons filter(p => p.age > 20) map(p => p.name)

//example which gave us pairs of primes
for{
  i <- 1 until n
  j <- 1 until i
  if isPrime(i+j)
} yield (i, j)

def ScalarProd(xs: Vector[Double], ys: Vector[Double]) : Double = {
  (for {
    x <- xs
    y <- ys
  } yield x * y
    ).sum
}
ScalarProd(Vector(1,2,3), Vector(1,2,3))



