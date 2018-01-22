package recfun

import java.util._

import scala.List

object Main extends App {
  def main() {
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
  def pascal(col: Int, row: Int): Int = {
    def factorial(n: Int): Int = {
      if (n <= 1)
        n
      else
        n * factorial(n - 1)
    }

    if (col > row + 1)
      throw new IllegalArgumentException("Column count is exceeded for row " + row + ". Max number" +
        "of row elements: " + (col + 1))
    else {
      factorial(row) / ((if (factorial(col) == 0) 1 else factorial(col)) *
        (factorial(if ((row - col) == 0) 1 else (row - col))))
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val listOfParentheses = chars.filter(x => (x == "(" | x == ")"))
    if (listOfParentheses.size % 2 != 0)
      throw new Exception("Parenthesis are unbalanced: there are not equal number of lefts and rights")
    else {
      return balanceImpl(chars, new Stack[Char])
    }
  }

  def balanceImpl(chars: List[Char], stack: Stack[Char]): Boolean = {
    if (chars.isEmpty)
      return stack.isEmpty
    else {
      chars.head match {
        case '(' => {
          stack.push(chars.head)
          balanceImpl(chars.tail, stack)
        }
        case ')' => {
          if (stack.size() > 0 && isMatch(stack.lastElement(), chars.head)) {
            stack.pop()
            balanceImpl(chars.tail, stack)
          }
          else
            return false
        }
        case _ => balanceImpl(chars.tail, stack)
      }
    }
  }

  def isMatch(left: Char, right: Char): Boolean = {
    if (left == "(".toCharArray()(0))
      right == ")".toCharArray()(0)
    else
      false
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int], index: Int): Int = {
    if (money < 0) return 0
    if (money == 0) return 1
    if (index == coins.size && money > 0) return 0
    countChange(money - coins(index), coins, index) + countChange(money, coins, index + 1)

  }

}
