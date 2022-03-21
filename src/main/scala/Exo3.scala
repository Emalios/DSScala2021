import Exo2.Table

import scala.annotation.tailrec

object Exo3 {

  trait Instr
  case object Skip extends Instr
  case class Assign(nomvar: String, expr: ExprArith) extends Instr
  case class Seq(i1: Instr, i2: Instr) extends Instr
  case class If(cond: ExprBool, ithen: Instr, ielse: Instr) extends Instr

  trait ExprArith
  case class Number(i: Int) extends ExprArith
  case class Var(x: String) extends ExprArith
  case class Plus(e1: ExprArith, e2: ExprArith) extends ExprArith

  trait ExprBool
  case class Bool(b: Boolean) extends ExprBool
  //Ajout suite à la question 3
  case class Et(b1: ExprBool, b2: ExprBool) extends ExprBool
  case class Ou(b1: ExprBool, b2: ExprBool) extends ExprBool
  case class Non(b: ExprBool) extends ExprBool

  /*
  Question 1 (1 point) – Quelle est la représentation du programme suivant ?
    x := 3;
    if false then y := 3;

  Réponse:
    Seq(
      Affect(x, Number(3)),
      If(Bool(false), Affect(y, Number(3)), Skip)
    )
  */

  //Fonctions données:
  def stringOfEArith(e: ExprArith): String = e match {
    case Number(i) => i.toString
    case Plus(e1, e2) => stringOfEArith(e1) + " + " + stringOfEArith(e2)
    case Var(x) => x
  }
  def stringOfEBool(e: ExprBool): String = e match {
    case Bool(b) => b.toString
  }

  def main(args: Array[String]): Unit = {
    val test = Seq ( Assign ("x", Number (3)) ,
      Seq ( Assign ("y", Number (4)) ,
        If ( Bool ( true ) , Assign ("x", Plus ( Var ("x"), Number (1))),
          Assign ("y", Plus ( Var ("y"), Number (1)))
        )
      )
    )
    /*
    x := 3;
    if true then x := x +1;
     */
    println(calculerProg(test, List.empty))
  }

  //Question 2
  def afficherProgramme(program: Instr): Unit = {
    def afficherInstr(instr: Instr): String = instr match {
      case Assign(nomvar, expr) => nomvar + " := " + stringOfEArith(expr) + ";"
      case If(cond, ithen, Skip) => "if " + stringOfEBool(cond) + " then " + afficherInstr(ithen)
      case If(cond, ithen, ielse) => "if " + stringOfEBool(cond) + " then " + afficherInstr(ithen) + " else " + afficherInstr(ielse)
      case Seq(i1, i2) => afficherInstr(i1) + "\n" + afficherInstr(i2)
      case Skip => ""
    }
    println(afficherInstr(program))
  }

  //Question 4
  def calculerEBool(expBool: ExprBool): Boolean = expBool match {
    case Bool(b) => b
    case Et(b1, b2) => calculerEBool(b1) && calculerEBool(b2)
    case Non(b) => !calculerEBool(b)
    case Ou(b1, b2) => calculerEBool(b1) || calculerEBool(b2)
  }

  //Méthode de l'exo 2
  type Table = List[(String, Int)]

  @tailrec def accesTab(t: Table, x: String): Int = t match {
    case t::q if t._1 == x => t._2
    case t::q => accesTab(q, x)
    case Nil => throw new Exception("tnureibgfe")
  }

  def majTab(t: Table, x: String, i: Int): Table = {
    @tailrec def aux(t: Table, acc: Table): Table = t match {
      case t::q if t._1 == x => (x, i)::acc ++ q
      case t::q => aux(q, t::acc)
      case Nil => (x, i)::acc
    }
    aux(t, List.empty)
  }

  //Question 5
  def calculerEArith(e: ExprArith, t: Table): Int = e match {
    case Number(i) => i
    case Plus(e1, e2) => calculerEArith(e1, t) + calculerEArith(e2, t)
    case Var(x) => accesTab(t, x)
  }

  //Question 6
  def calculerProg(i: Instr, t: Table): Table = i match {
    case Assign(nomvar, expr) => majTab(t, nomvar, calculerEArith(expr, t))
    case If(cond, ithen, _) if calculerEBool(cond) => calculerProg(ithen, t)
    case If(_, _, ielse) => calculerProg(ielse, t)
    case Seq(i1, i2) => {
      val l1 = calculerProg(i1, t)
      val l2 = calculerProg(i2, l1)
      l2
    }
    case Skip => t
  }

}
