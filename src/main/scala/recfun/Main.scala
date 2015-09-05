package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

      def isBalanced(string: List[Char], opens: Int, closes: Int) : Boolean = {
        /*string match {
          case       Nil => opens == closes
          case '('::tail => isBalanced(tail, opens+1, closes)
          case ')'::tail => {
            if (closes > opens) {
              false //Don't work because infers false and enters later on case _::tail
            }
            else {
              isBalanced(tail, opens, closes+1)
            }
          }
          case   _::tail => isBalanced(tail, opens, closes)
        }*/

        if (string.isEmpty) opens == closes
        else if (closes > opens) false
        else if (string.head == '(') isBalanced(string.tail, opens+1, closes)
        else if (string.head == ')') isBalanced(string.tail, opens, closes+1)
        else isBalanced(string.tail, opens, closes)
      }

    isBalanced(chars, 0, 0)
  }

  /**
   * Exercise 3
   *
   * Sum the ways of change of
   * a) the money having used the first coin(money-coin) and the same subset
   * b) the ways the same money without the money used
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty && money >=1) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins )
  }

}
