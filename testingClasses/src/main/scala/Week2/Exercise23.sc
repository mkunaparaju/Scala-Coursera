// to get the fixed point of a function
object exercise23 {
  val tolerance = 0.001
  def isCloseEnough(x: Double, y: Double) = {
    Math.abs((x - y) / x) / x < tolerance
  }
  def fixedPoint (f : Double => Double) (firstGuess : Double) = {
    //this method tests whether our guess isclose enough to
    // the result or else we iterate with the next as the new parameter
    def iterate (guess : Double) : Double = {
      //println(guess)
      val next = f(guess)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => (1+ (x/2)))(1)
  // this impl doesnt work as the successive values are too far apart
  //def sqrt(x: Double) = fixedPoint(y => x/y) (1.0)
  // here we take the average of two successive values
  def sqrt(x: Double) = fixedPoint(y => (y + (x/y))/2) (1.0)
  sqrt(4)

  def avgDamp (f: Double => Double) (x:Double) = (x + f(x)) / 2

  def sqrtWAvgDamp(x: Double) = fixedPoint(avgDamp(y => y / x))(1)
  //sqrtWAvgDamp(4)


}