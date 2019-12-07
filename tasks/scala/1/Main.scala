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
    if (c == 0 || c == r) {
      1
    }
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    balancingHelper(chars, 0)
  }

  def balancingHelper(chars: List[Char], counter: Int): Boolean = {
    if (chars.isEmpty) {
      counter == 0
    }
    else if (chars.head == '(') {
      balancingHelper(chars.tail, counter + 1)
    }
    else if (chars.head == ')')
      (counter > 0) && balancingHelper(chars.tail, counter - 1)
    else {
      balancingHelper(chars.tail, counter)
    }
  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    }
    else if (money < 0 || coins.isEmpty) {
      0
    }
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
