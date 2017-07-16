//place 8 Queens on a chessboard so
//no queen is threthened by another

//no two queens in the same row, col, diag
//n is then number of queens
def queens(n: Int) : Set[List[Int]] = {
  def placeQueens(k : Int) : Set[List[Int]] = {
    if (k == 0) Set(List())
    else {
      for {
        queens <- placeQueens(k-1)
        col <- 0 until n
        if isSafe(col, queens)
      }yield col :: queens
    }
  }
  placeQueens(n)
}


def isSafe(col: Int, queen: List[Int]) : Boolean = {
  val row = queen.length
  val queenWithRow = (row - 1 to 0 by -1 ) zip queen
  queenWithRow forall {
    case (r, c) => ((col != c) && math.abs(col - c) != row - r )
  }
}

queens(4)
