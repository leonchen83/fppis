package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
    println(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    c match {
      case 0 => 1
      case `r` => 1
      case x => pascal(x - 1, r - 1) + pascal(x, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def rec(list: List[Char], balance: Int): Int = {
      (list, balance) match {
        case (_, -1) => -1
        case (list, _) =>
          ((list: List[Char], balance: Int) => {
            list match {
              case Nil => balance
              case head :: tail => {
                head match {
                  case '(' => rec(tail, balance + 1)
                  case ')' => rec(tail, balance - 1)
                  case _ => rec(tail, balance)
                }
              }
            }
          })(list, balance)
      }
    }
    val rs = rec(chars, 0)
    rs == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money,coins) match {
      case (0,_) =>1
      case (a,_) if a<0 =>0
      case (_,a) if a.isEmpty =>0
      case (a,head::tail) => countChange(a - head, head::tail) + countChange(a, tail)
    }
  }
}
