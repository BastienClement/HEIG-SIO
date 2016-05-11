import scala.language.implicitConversions

package object util {
	/**
	  * Ajoute des méthodes statistiques sur les séquences de doubles.
	  */
	implicit class StatsOps(val seq: Seq[Double]) extends AnyVal {
		/** Moyenne */
		@inline def mean: Double = seq.sum / seq.length

		/** Variance */
		@inline def variance: Double = {
			val m = mean
			seq.foldLeft(0.0) { (acc, xk) =>
				val dk = xk - m
				acc + dk * dk
			} / seq.length
		}

		/** Ecart-type */
		@inline def stdev: Double = Math.sqrt(variance)
	}

	/**
	  * Transforme implicitement une variable de type T en type Double si un objet
	  * Numeric[T] est implicitement disponible.
	  * Permet de simplifier l'utilsiation de types génériques numériques.
	  */
	@inline implicit def NumericToDouble[T](n: T)(implicit ev: Numeric[T]): Double = ev.toDouble(n)
}
