// sum of all values between a and b

def sumValues (a: Int, b: Int) : Int = {
  if(a < b) 0 else a + sumValues(a+1, b)
}

def cube (x: Int) : Int = x*x*x

def sumCubes (a: Int, b: Int ) : Int = {
  if (a < b) 0 else cube(a) + sumCubes(a+1, b)
}

def sum(f: Int => Int, a: Int, b: Int ) : Int = {
  if(a > b) 0 else f(a) + sum(f, a+1, b)
}
//these below functions can be used as params in the sum function
def id(x: Int) : Int = x
def factorial(x: Int) : Int = {
  if(x == 0 ) 1 else x*factorial(x-1)
}

sum(cube, 1,3)

//using anonymous functions
def sumIntsAnonymous(a: Int, b: Int) :Int = sum(x => x, a ,b)
def sumCubeAnonymous(a: Int, b: Int) :Int = sum(x => x*x*x, a ,b)

def sumRecurse(f : Int => Int, a: Int, b: Int) : Int = {
  def loop (a: Int, acc: Int) : Int = {
    if(a > b) acc
    else loop (a+1, acc+f(a))
  }
  loop (a, 0)
}
// calling the function defined
sumRecurse(cube, 1,3)
//calling using an anonymous function
sumRecurse(x => x*x, 1,3)


////// Currying ////////////////

def sumCurry(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int) : Int = {
    if( a > b ) 0
    else f(a) + sumF(a+1, b)
  }
  sumF
}

def sumIntsCurry = sumCurry(x => x)
def sumCubeCurry = sumCurry(x => x*x*x)
def sumFactCurry = sumCurry(factorial)

//sumcurry(cube) is applies sumcurry to cubes function and returns the sum of
// cubes function
//that function is then applies to args 1, 3
//sumCurry (cube) (a,b) == (sumCurry(cube))(a,b)
sumCurry(cube)(1,3)

//shortened form
def sumShortCurry (f: Int => Int) (a: Int, b: Int) : Int = {
  if( a > b) 0 else f(a) + sumShortCurry(f)(a+1, b)
}


// returns the product of an interval

def product(f : Int => Int) (a: Int, b: Int) : Int = {
  if(a > b) 1
  else f(a) * product(f)(a+1, b)
}

product(x => x*x) (1,4)

//writing a fatorial in terms of product function
def factAlternative(n: Int) = product(x => x) (1, n)
factAlternative(5)

// generalizes both sum and product


def mapReduce(f : Int => Int , combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int) : Int = {
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
}

// now define product using mR
def productMR(f : Int => Int) (a: Int, b: Int) : Int =
  mapReduce(f, (x, y) => x*y, 1 )(a,b)

productMR(x=> x*x) (1,3)

type Set = Int => Boolean
def Singelton(elem : Int) = (x:Int) => x == elem


