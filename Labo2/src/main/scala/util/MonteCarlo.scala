package util

/**
  * Objet utilitaire pour générer des moyennes et des intervalles de confiance
  * basées sur la méthode de Monte-Carlo.
  */
object MonteCarlo {
	/** Coefficient de confiance */
	val z95 = 1.960

	/** Un intevalle de confiance */
	case class ConfidenceInterval(min: Double, max: Double) {
		/** L'écart entre les deux bornes de l'intervalle */
		val delta = max - min
	}

	/** Résulat d'une simulation pour une valeur */
	case class Result(mean: Double, sd: Double, ci: ConfidenceInterval)

	/**
	  * Effectue les calculs statistiques pour fabriquer une instance à partir
	  * d'une suite de réalitions.
	  */
	def stats(realizations: Seq[Double]): Result = {
		// Nombre de réalisations
		val n = realizations.length

		// Moyenne
		val mean = realizations.mean

		// Estimation de l'écart-type
		val s = realizations.stdev

		// Un demi-intervalle de confiance
		val d = z95 * (s / Math.sqrt(n))

		Result(mean, s, ConfidenceInterval(mean - d, mean + d))
	}

	/**
	  * Produit un résultat de simulation basé sur n réalisation d'une variable aléatoire.
	  *
	  * @param n       nombre de réalisation à calculer
	  * @param compute fonction sans paramètre générant des réalisations
	  */
	def run(n: Int)(compute: => Double): Result = stats(for (i <- 1 to n) yield compute)

	/**
	  * Produit plusieurs résultats de simulation basé sur n réalisation
	  * de plusieurs variables aléatoires.
	  *
	  * @param n       nombre de réalisation à calculer
	  * @param compute fonction sans paramètre générant des réalisations
	  *                chaque élément de la séquence générée correspond à une variable aléatoire
	  */
	def multirun(n: Int)(compute: => Seq[Double]): Seq[Result] = {
		// Génération de réalisations
		val results = for (i <- 1 to n) yield compute

		// Transpose la séquence pour obtenir une séquence par variable aléatoire
		val variables = results.transpose

		// Calcul des statistiques pour chaque variable indépendament
		variables.map(stats)
	}
}
