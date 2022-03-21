import scala.annotation.tailrec

object Exo2 {

  type Table = List[(String, Int)]

  /*
  On représente une table qui associe à une chaîne un entier par une liste de couples.
  type Table = List[(String, Int)]
Pour les questions suivantes, on suppose qu’une chaîne apparaît au plus une fois dans une table, et que cette
condition est respectée dans les tables passées en paramètre.
Question 1 (1 point) – Écrire la fonction accesTab qui prend en paramètre une table t et une chaîne x et
retourne l’entier associé à x dans t. La fonction retourne une erreur si la chaîne n’est pas dans la table.
Question 2 (2 points) – Écrire la fonction majTab(t:Table, x:String, i:Int):Table qui retourne une copie
de t dans laquelle x est associée à i. Si x est déjà dans t, la valeur est remplacée, sinon, un nouveau couple est
ajouté.

   */

  def main(args: Array[String]): Unit = {

  }

  @tailrec def accesTab(t: Table, x: String): Int = t match {
    case t::q if t._1 == x => t._2
    case t::q => accesTab(q, x)
    case Nil => throw new Exception("ratio")
  }

  def majTab(t: Table, x: String, i: Int): Table = {
    @tailrec def aux(t: Table, acc: Table): Table = t match {
      case t::q if t._1 == x => (x, i)::acc ++ q
      case t::q => aux(q, t::acc)
      case Nil => (x, i)::acc
    }
    aux(t, List.empty)
  }

}
