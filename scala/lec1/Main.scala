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
  def pascal(c: Int, r: Int): Int = 
    if ((c == 0) || (c == r)) 1 
    else pascal(c-1, r-1) + pascal(c, r-1)   

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	def balance_core(chars: List[Char], left_num: Int): Boolean =
      if (chars.isEmpty) left_num == 0
      else if ((chars.head != '(') && (chars.head != ')'))
        balance_core(chars.tail, left_num);
      else if (chars.head == '(')
        balance_core(chars.tail, left_num+1)
      else if (left_num == 0)
        false
      else
        balance_core(chars.tail, left_num-1)
    balance_core(chars, 0)    
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (! coins.isEmpty) {
      val n = money/coins.head;
      {for (i <- 0 to n) yield countChange(money - i*coins.head, coins.tail)}.sum
    } else 0
  }
}
