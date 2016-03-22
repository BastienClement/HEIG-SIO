package util

import func.FunctionDefinition
import gen.RealizationGenerator
import java.util
import scala.util.Random

/** Helper for generating random values */
object Rand {
	/** Generates a random Double value between [0; 1[ */
	def nextDouble(implicit random: Random): Double = random.nextDouble()

	/** Generates a random Double value between [origin; bound[ */
	def nextDouble(origin: Double, bound: Double)(implicit random: Random): Double = {
		val r = nextDouble * (bound - origin) + origin

		// Correct possible double rounding error
		// See: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
		if (r >= bound) Math.nextDown(r) else r
	}

	/** Construct a discrete generator returning slices from a function */
	def sliceGenerator(fd: FunctionDefinition)(implicit random: Random) = discreteGenerator(fd.slicesLaw)

	/**
	  * Constructs a discrete number generator following the given probability table.
	  * Based on the Alias Method:
	  * - http://www.keithschwarz.com/darts-dice-coins/
	  * - http://www.keithschwarz.com/interesting/code/?dir=alias-method
	  */
	def discreteGenerator[T](law: Seq[(Double, T)])(implicit random: Random): RealizationGenerator[T] = {
		if (!law.hasDefiniteSize) throw new IllegalArgumentException("The law table must have a finite length")

		var (probabilities, values) = law.toVector.unzip
		val n = values.length

		// Allocate space for the probability and alias tables.
		val probability = new Array[Double](n)
		val alias = new Array[Int](n)

		// Compute the average probability and cache it for later use.
		val average = 1.0 / n

		// Create two stacks to act as worklists as we populate the tables.
		val small, large = new util.ArrayDeque[Int]()

		// Populate the stacks with the input probabilities.
		for (i <- probabilities.indices) {
			// If the probability is below the average probability, then we add
			// it to the small list; otherwise we add it to the large list.
			val d = if (probabilities(i) >= average) large else small
			d.add(i)
		}

		/* As a note: in the mathematical specification of the algorithm, we
		will always exhaust the small list before the big list.  However,
		due to floating point inaccuracies, this is not necessarily true.
		Consequently, this inner loop (which tries to pair small and large
		elements) will have to check that both lists aren't empty. */
		while (!small.isEmpty && !large.isEmpty) {
			// Get the index of the small and the large probabilities.
			val less = small.removeLast()
			val more = large.removeLast()

			// These probabilities have not yet been scaled up to be such
			// that 1/n is given weight 1.0.  We do this here instead.
			probability(less) = probabilities(less) * n
			alias(less) = more

			// Decrease the probability of the larger one by the appropriate amount.
			probabilities = probabilities.updated(more, (probabilities(more) + probabilities(less)) - average)

			// If the new probability is less than the average, add it into the
			// small list; otherwise add it to the large list.
			val d = if (probabilities(more) >= average) large else small
			d.add(more)
		}

		/* At this point, everything is in one list, which means that the
		remaining probabilities should all be 1/n.  Based on this, set them
		appropriately.  Due to numerical issues, we can't be sure which
		stack will hold the entries, so we empty both. */
		while (!small.isEmpty) probability(small.removeLast()) = 1.0
		while (!large.isEmpty) probability(large.removeLast()) = 1.0

		new RealizationGenerator[T] {
			override def produce(): T = {
				// Generate a fair die roll to determine which column to inspect.
				val column = random.nextInt(n)

				// Generate a biased coin toss to determine which option to pick.
				val toss = random.nextDouble() < probability(column)

				// Based on the outcome, select either the column or its alias.
				val choice = if (toss) column else alias(column)

				// Return an actual value from the law
				values(choice)
			}
		}
	}
}
