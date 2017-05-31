package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 5) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if ( r == 0) 1
      if(c == 0 || c ==r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
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


  /**
   * Exercise 3
   */



  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
  }

