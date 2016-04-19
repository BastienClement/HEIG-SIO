package func

import func.PiecewiseAffineFunction.Slice
import scala.annotation.tailrec
import util.DiscreteGenerator.Law
import util.NumericToDouble

/**
  * Objet compagnion pour la classe FunctionDefinition
  */
object PiecewiseAffineFunction {
	/**
	  * Constructeur prenant comme paramètres un nombre variable de points
	  */
	def apply[T: Numeric, U: Numeric](points: (T, U)*) = new PiecewiseAffineFunction(slicesFromPoints(points))

	/**
	  * Constructeur prenant comme paramètre une séquence de points à utiliser
	  */
	def apply[T: Numeric, U: Numeric](slices: TraversableOnce[(T, U)]) = new PiecewiseAffineFunction(slicesFromPoints(slices))

	/**
	  * Construit un tableau de Slice à partir d'une liste de points.
	  */
	def slicesFromPoints[T: Numeric, U: Numeric](points: TraversableOnce[(T, U)]): Array[Slice] = {
		// Groupe chaque point avec son voisin en utilisant une fenêtre glissante de largeur 2
		// --> (1, 2) (2, 3) (3, 4) ...
		val pairs = points.toSeq.sliding(2)

		// Transforme chaque paire de point en Slice
		val slices = for (Seq((x0, y0), (x1, y1)) <- pairs) yield Slice(x0, y0, x1, y1)

		// Retourne un tableau pour maximiser les performances d'accès
		slices.toArray
	}

	/**
	  * Une tranche de la fonction.
	  *
	  * La plupart des calculs effectués par cette classe sont basés sur
	  * les réponses aux question de l'exercice 1 du TP1.
	  */
	case class Slice(x0: Double, y0: Double, x1: Double, y1: Double) {
		// Vérification des contraintes
		if (x0 >= x1) throw new IllegalArgumentException(s"x0 [$x0] must be less than x1 [$x1]")
		if (y0 < 0 || y1 < 0) throw new IllegalArgumentException(s"y0 [$y0] and y1 [$y1] must be greater or equal to 0")

		/** Pente de cette tranche de fonction */
		val m = (y1 - y0) / (x1 - x0)

		/**
		  * L'aire entre la courbe de la fonction et l'axe des x, entre x0 et x1.
		  *
		  * Au lieu de calculer l'intégrale définie, nous utilisons ici le fait que
		  * la tranche défini également un trapèze dont l'aire est bien plus simple
		  * à calculer.
		  */
		val area = (x1 - x0) * (y1 + y0) / 2

		/**
		  * L'espérance d'une variable aléatoire qui utiliserait une version
		  * proportionnelle de cette fonction en tant que fonction de densité.
		  * Si cette fonction ne peut pas former une fonction de densité (son aire
		  * est nulle) cette fonction retourne NaN puisqu'une telle variable
		  * aléatoire ne peut exister.
		  */
		val expectedValue = (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1)) / (3 * (y0 + y1))

		/**
		  * La tranche est non-nulle
		  */
		val nonNull = y0 != 0.0 || y1 != 0.0

		/**
		  * Vérifie si une valeur de x est contenue dans la tranche.
		  */
		def contains(x: Double) = x >= x0 && x <= x1

		/**
		  * Evalue la section de fonction de cette tranche pour un x donné.
		  * Cette méthode ne doit pas être appelée avec des valeurs de x en dehors
		  * de la tranche.
		  */
		def apply(x: Double) = m * (x - x0) + y0
	}

}

/**
  * Définition d'une fonction affine par morceau en tant qu'aggrégat de tranches.
  * Le constructeur de cette classe est privé, il est nécessaire d'utiliser les constructeurs
  * de l'objet compagnion pour créer une instance de cette classe.
  *
  * @param slices les tranches de la fonction
  */
class PiecewiseAffineFunction private(val slices: Array[Slice]) {
	// Au moins une tranche est définie
	if (slices.length < 1) throw new IllegalArgumentException("Function definition requires at least one slice")

	// Au moins une tranche possède une aire non-nulle
	if (!slices.exists(slice => slice.nonNull)) throw new IllegalArgumentException("At least one yk must be non-zero")

	/** f(x) = 0 pour tout x < a */
	val a = slices.head.x0

	/** f(x) = 0 pour tout x > b */
	val b = slices.last.x1

	/** La valeur maximale de la fonction */
	val ym = slices.map(slice => slice.y0 max slice.y1).max

	/**
	  * L'aire entre l'axe des x et la courbe de la fonction.
	  * Définie comme la somme de l'aire de chaque tranche.
	  */
	lazy val area: Double = slices.map(slice => slice.area).sum

	/**
	  * Une loi discrète associant à chaque tranche une probabilité égale au
	  * ratio de son aire par rapport à l'aire totale de la fonction.
	  */
	lazy val slicesLaw: Law[Slice] = slices.map(slice => (slice.area / area, slice))

	/**
	  * Calcul l'espérance d'une variable aléatoire qui utiliserait une version
	  * proportionnelle de cette fonction comme fonction de densité.
	  */
	lazy val expectedValue = slicesLaw.collect { case (pk, slice) if slice.nonNull => pk * slice.expectedValue }.sum

	/**
	  * Retourne la tranche de la fonction contenant la valeur x donnée.
	  */
	def sliceFor(x: Double): Slice = {
		// Recherche dichotomique
		// Cette fonction est tail-recursive et sera optimisée en simple boucle par le compilateur
		@tailrec def search(lo: Int, hi: Int): Slice = {
			if (lo > hi) throw new IllegalArgumentException(s"Function is undefined for x = $x")

			val mid = (lo + hi) / 2
			val slice = slices(mid)

			if (slice.contains(x)) slice
			else if (x < slice.x0) search(lo, mid - 1)
			else search(mid + 1, hi)
		}

		// On commence avec le tableau entier
		search(0, slices.length - 1)
	}

	/**
	  * Evalue la fonction pour une valeur de x donnée.
	  */
	def apply(x: Double) = sliceFor(x).apply(x)

	/**
	  * Returns a new FunctionDefinition (proportional to this one) with an area of 1,
	  * suitable to be used as a density function.
	  */
	lazy val proportionalDensity: PiecewiseAffineFunction = {
		/**
		  * Scales a slice of this function by the area of this function.
		  * In practice, we create a new slice with scaled y0 and y1 values.
		  */
		def scaleSlice(slice: Slice) = slice.copy(y0 = slice.y0 / area, y1 = slice.y1 / area)

		new PiecewiseAffineFunction(slices.map(scaleSlice)) {
			// Return the same object, since the function is already a density function. :)
			override lazy val proportionalDensity: PiecewiseAffineFunction = this
		}
	}
}
