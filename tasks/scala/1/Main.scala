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
    if (c > r) {
      throw new IllegalArgumentException("Column must be fewer than row");
    }

    if (c == 0 || c == r) {
      return 1
    }

    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        count == 0
      } else if (chars.head == '(') {
        check(chars.tail, count + 1)
      } else if (chars.head == ')') {
        (count > 0) && check(chars.tail, count - 1)
      } else {
        check(chars.tail, count)
      }
    }

    check(chars, 0)
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
      1;
    } else if ((money < 0) || coins.isEmpty) {
      0;
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

  }
}
