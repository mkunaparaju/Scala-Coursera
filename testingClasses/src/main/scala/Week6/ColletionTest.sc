val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val s = "hello ScAla"
s filter (c => c.isLower )
s exists (c => c.isUpper)
s forall (c => c.isUpper)


val r : Range = 1 until 5
val p : Range = 1 to 5
1 to 10 by 3
6 until 0 by -2

//zip returns a single sequence orf pairs
//of simeltaneous elements from 2 given pairs
xs zip s

s flatMap (c => List('.', c))

//examples
//list all comb of nums x and y where
//x is from 1-M, y is from 1-N
val M = 10
val N = 10
(1 to M) flatMap(x => (1 to N) map (y => (x,y)))

//Scalar Product
def ScalarProd(xs: Vector[Double], ys: Vector[Double]) : Double = {
  (xs zip ys).map(xy => xy._1 * xy._2).sum
}

def scalarProdMatch(xs: Vector[Double], ys: Vector[Double]) : Double = {
  (xs zip ys).map{case(x, y) => x*y}.sum
}

//test prime
def isPrime(n : Int) : Boolean = (2 until n) forall(x => (n%x) != 0)
