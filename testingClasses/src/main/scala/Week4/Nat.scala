package Week4

/**
  * Created by mkunaparaju on 6/17/2017.
  */

//peano numbers

abstract class Nat {
  def isZero : Boolean
  def predecessor: Nat
  def successsor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")

  //override def successsor: Nat = new Succ(this)

  def +(that: Nat) = that

  def -(that: Nat) = if(that.isZero) this else throw new Error("negetive number")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  //class signature is such that
  // we are passing in the predecesson
  override def predecessor: Nat = n

  //override def successsor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else  (n - that.predecessor)
}
