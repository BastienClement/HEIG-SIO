package util

import java.util.Random

/**
  * Une sous-classe de Random ajoutant des méthodes utiles dans le cas du laboratoire.
  */
trait ExtendedRandom extends Random {
	/** Génère un double entre [origin; bound[ */
	@inline final def nextDouble(origin: Double, bound: Double): Double = {
		val r = nextDouble() * (bound - origin) + origin

		// Possible erreur liée à l'imprécision des doubles
		// See: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
		if (r >= bound) Math.nextDown(r) else r
	}

	/** Génère un entier entre [origin; bound[ */
	@inline final def nextInt(origin: Int, bound: Int): Int = nextInt(bound - origin) + origin
}
