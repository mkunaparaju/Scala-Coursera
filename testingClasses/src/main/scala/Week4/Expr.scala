package Week4

/**
  * Created by mkunaparaju on 6/22/2017.
  */
trait Expr

case class Num(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr


