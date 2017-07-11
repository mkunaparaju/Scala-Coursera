def sqRoot(x: Double) : Double = {
  //auxilarry functions can be defined inside main function
  //they will not be visible outside users
  def squareIter(x: Double, guess: Double) : Double = {
    if(isGoodEnough(x, guess)) guess
    else squareIter(x, improveGuess(x, guess))
  }

  def isGoodEnough(x: Double, guess: Double) : Boolean = {
    math.abs((guess*guess) - x) <= 0.001*x
  }

  def improveGuess(x: Double, guess:Double) =   ((x/guess) + guess)/2

  squareIter(x, 1.0)
}



sqRoot(2)
sqRoot(4)

sqRoot(1e-6)
sqRoot(1e6)


def gcd(a: Int, b: Int) : Int = {
  if(b==0) a else gcd(b, a%b)
}

gcd(85, 34)

def factorial(n : Int) : Int = {
  if(n == 0) 1 else n * factorial(n-1)
}

factorial(4)


def tailRecurseFactorial(n: Int) : Int = {
  def loop (n : Int, acc : Int) : Int = {
    if( n == 0 ) acc
    else loop(n-1, acc*n)
  }
  loop(n,1)
}

tailRecurseFactorial(5)


def balance(chars : List[Char]) : Boolean = {
  def balanceAux(chars : List[Char], netOpenParen : Int) : Boolean = {

    if(chars.isEmpty) (netOpenParen == 0)
    else
    {
      var aux = 0

      if (chars.head == '(') aux = netOpenParen + 1
      else if (chars.head == ')') aux = netOpenParen - 1
      else aux = netOpenParen

      if (aux >= 0) balanceAux(chars.tail, aux)
      else false

    }
  }
  balanceAux(chars, 0 )
}

balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)



countChange(300,List(5,10,20,50,100))











