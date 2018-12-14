package recfun

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
  def pascal(column: Int, row: Int): Int = {
    column match {
      case 0 => 1
      case col: Int if col == row => 1
      case col: Int if col > 0 && col < row => pascalRecursive(row, col - 1)
      case _ => throw new Exception("Invalid parameter")
    }
  }

  def pascalRecursive(row: Int, column: Int): Int = {
    if (column != 0) {
      pascalRecursive(row, column - 1) * (row - column) / (column + 1)
    } else {
      row
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = ???

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
