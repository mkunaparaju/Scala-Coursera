import Week4.Expr

  def show(e: Expr): String = e match {
    case Num(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }


show( Sum(Num(1), Num(2)))



