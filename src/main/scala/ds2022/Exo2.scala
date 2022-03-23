package ds2022

import scala.annotation.tailrec

object Exo2 {

  //Question 1
  def afficher(n: Int): Unit = {
    @tailrec def aux(acc: Int): Unit = {
      if(acc <= n) printLine(acc)
      if(acc != n) aux(acc + 1)
    }
    def printLine(i: Int): Unit = {
      if(i == n) println(i)
      if (i < n) {
        print(i + " ")
        printLine(i + 1)
      }
    }
    aux(1)
  }

  def main(args: Array[String]): Unit = {
    afficher(5)
  }

}
