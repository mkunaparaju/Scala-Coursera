// has 2 members numer and denom
class Rational (x: Int, y: Int) {
  def numer = x
  def denom = y
  //takes only a single param
  //left operand is that rational itself
  def add(that: Rational) = new Rational(numer * that.denom + that.numer * denom , denom * that.denom)

  def neg = new Rational((numer - (2 * numer)), denom)
  //advantage is to not repeat oneself
  def sub(that: Rational)   =   add(that.neg)
  override def toString: String = numer + "/" + denom
}


val x =  new Rational(1,3)
val y =  new Rational(5,7)
val z =  new Rational(3,2)
x.add(y)
x.neg
x.sub(y).sub(z)



def addition(n1: Rational, n2: Rational) : Rational =
  new Rational((n1.numer * n2.denom + n2.numer * n1.denom) , (n1.denom*n2.denom))
//converts to a string
def makeString (r: Rational) = r.numer + "/" + r.denom

makeString(addition(x,y))







