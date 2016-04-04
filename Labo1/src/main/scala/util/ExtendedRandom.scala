package util

import java.util.Random

trait ExtendedRandom extends Random {
	/**
	  * Generates a random Double value between [origin; bound[
	  */
	@inline def nextDouble(origin: Double, bound: Double)(implicit random: Random): Double = {
		val r = nextDouble() * (bound - origin) + origin

		// Correct possible double rounding error
		// See: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
		if (r >= bound) Math.nextDown(r) else r
	}

	/**
	  * Generates a random Int value between [origin; bound[
	  */
	def nextInt(origin: Int, bound: Int)(implicit random: Random): Int = random.nextInt(bound - origin) + origin
}
