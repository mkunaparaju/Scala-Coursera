package Week3

/**
  * Created by mkunaparaju on 6/9/2017.
  */
class Rational (x: Int, y: Int)  {

  //enforces a precondition
  require(y != 0, "denominator must be nonzer")

  //if this keyword is used in a function pos
  //then this is another constructor of the class
  //2 arg constructor is the implicit constructor
  def this(x: Int) = this(x, 1)

  // for simplification of the rational
  //donot want clients to see gcd - strictly for implementation
  private def gcd(a: Int, b: Int) : Int = {
    if(b==0) a else gcd(b, a%b)
  }

  private val g = gcd(x,y)

  //def numer = x/g
  //def denom = y/g

  //change numer and denom to vals computed only once as soon as
  //time will be amortized and they wont need to be computed
  //all the time
  val numer = x/g
  val denom = y/g

  //takes only a single param
  //left operand is that rational itself
  //def add (that: Rational) = new Rational(numer * that.denom + that.numer * denom , denom * that.denom)
  def + (that: Rational) = new Rational(numer * that.denom + that.numer * denom , denom * that.denom)

  // negitive of a number
  // needs only one member
  //def neg  = new Rational((numer - (2 * numer)), denom)
  def unary_-  = new Rational((numer - (2 * numer)), denom)

  //advantage is to not repeat oneself
  //def sub (that: Rational)   =   this + -that
  def - (that: Rational)   =   this + -that

  //def less (that: Rational) = numer * that.denom < denom * that.numer
  def < (that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if(this < that) that else this
  override def toString: String = numer + "/" + denom

}
