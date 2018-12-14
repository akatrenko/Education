package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("s((Hello (recursive)))".toList))
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

  private def pascalRecursive(row: Int, column: Int): Int = {
    if (column != 0) {
      pascalRecursive(row, column - 1) * (row - column) / (column + 1)
    } else {
      row
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    balanceRecursive(chars.filter(char => char.equals('(') || char.equals(')'))) == 0
  }

  private def balanceRecursive(chars: List[Char]): Int = {
    chars match {
      case Nil => 0
      case x :: tail =>
        val ss = if (x.equals('(') && tail.nonEmpty) 1 else -1
        ss + balanceRecursive(tail)
    }
  }

  /**
    * Another decision
    *
    * To start you need balanceRecursive(resList.tail, 1)
    *
    * @param chars input list
    * @return result score
    */
  /*@tailrec
  private def balanceRecursive(chars: List[Char], res: Int): Boolean = {
    if (chars.nonEmpty) {
      val symb = if(chars.head.equals('(')) 1 else -1
      if(chars.tail.isEmpty && symb == 1) false else balanceRecursive(chars.tail, res + symb)
    } else {
      res == 0
    }
  }*/

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
