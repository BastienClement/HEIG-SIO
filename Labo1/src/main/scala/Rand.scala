import scala.util.Random

/** Helper for generating random values */
object Rand {
	/** Generates a random Double value between [0; 1[ */
	def nextDouble(implicit random: Random): Double = random.nextDouble()

	/** Generates a random Double value between [origin; bound[ */
	def nextDouble(origin: Double, bound: Double)(implicit random: Random): Double = {
		val r = nextDouble * (bound - origin) + origin

		// Correct potential double rounding error
		// See: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
		if (r >= bound) Math.nextDown(r) else r
	}
}
