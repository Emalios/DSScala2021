package ds2022

object Exo3 {

  trait Feuillage
  case object Feuille extends Feuillage
  case class Fourche(gauche: Feuillage, droite: Feuillage) extends Feuillage
  case class Branche(f: Feuillage) extends Feuillage

  trait Arbre
  case class Tronc(t: Arbre) extends Arbre
  case class Couronne(f: Feuillage) extends Arbre

  //Question 1
  val arbreDroite = Tronc(Couronne(Branche(Fourche(Branche(Feuille), Fourche(Feuille, Feuille)))))

  //Question 2
  def estArbuste(a: Arbre): Boolean = a match {
    case Couronne(f) => true
    case Tronc(t) => false
  }

  //Question 3
  def hauteur(a: Arbre): Int = {
    def auxA(acc: Int, a: Arbre): Int = a match {
      case Couronne(f) => auxF(acc, f)
      case Tronc(t) => auxA(acc + 1, t)
    }
    def auxF(acc: Int, f: Feuillage): Int = f match {
      case Branche(f) => auxF(acc + 1, f)
      case Feuille => acc
      case Fourche(gauche, droite) => max(auxF(acc + 1, gauche), auxF(acc + 1, droite))
    }
    def max(i1: Int, i2: Int): Int = if (i1 > i2) i1 else i2
    auxA(0, a)
  }

  //Question 4
  def estEquilibre(f: Feuillage): Boolean = {
    def aux(acc: Boolean, f: Feuillage): Boolean = f match {
      case Branche(f) => false
      case Feuille => true
      case Fourche(gauche, droite) => acc && estEgal(gauche, droite)
    }
    def estEgal(f1: Feuillage, f2: Feuillage): Boolean = {
      if (hauteur(Couronne(f1)) == hauteur(Couronne(f2))) {
        aux(true, f1) && aux(true, f2)
      } else false
    }
    aux(true, f)
  }

  //Question 5
  def creerFeuillageEquilibre(n: Int): Feuillage = n match {
    case 1 => Feuille
    case _ => Fourche(creerFeuillageEquilibre(n - 1), creerFeuillageEquilibre(n - 1))
  }

  //Question 6
  def transformerListeEnArbre(l: List[Char]): Arbre = {
    def auxA(l: List[Char]): Arbre = l match {
      case 't'::q => Tronc(transformerListeEnArbre(q))
      case 'c'::q => Couronne(auxF(q))
    }
    def auxF(l: List[Char]): Feuillage = l match {
      case 'f'::q => {
        val (l, reste) = getProchaineFeuille(q)
        Fourche(auxF(l), auxF(reste))
      }
      case 'b'::q => Branche(auxF(q))
      case 'l'::q => Feuille
    }
    def getProchaineFeuille(l: List[Char]): (List[Char], List[Char]) = {
      def aux(l: List[Char], acc: List[Char]):  (List[Char], List[Char]) = l match {
        case 'l'::q => {
          (acc ++ List('l'), q)
        }
        case t::q => {
          aux(q, t::acc)
        }
      }
      aux(l, List.empty)
    }
    auxA(l)
  }

  val aEquilibre = Fourche(Fourche(Fourche(Feuille, Feuille), Fourche(Feuille, Feuille)), Fourche(Fourche(Feuille, Feuille), Fourche(Feuille, Feuille)))
  val aPasEquilibre = Fourche(Fourche(Fourche(Feuille, Feuille), Fourche(Feuille, Feuille)), Fourche(Fourche(Feuille, Feuille), Feuille))
  val aPasEquilibr2 = Fourche(Fourche(Fourche(Feuille, Feuille), Fourche(Feuille, Feuille)), Fourche(Fourche(Feuille, Feuille), Branche(Feuille)))

  val listeArbreGauche = 't'::'t'::'c'::'f'::'b'::'b'::'l'::'f'::'l'::'f'::'l'::'l'::Nil

  def main(args: Array[String]): Unit = {
    println(transformerListeEnArbre(listeArbreGauche))
  }

}
