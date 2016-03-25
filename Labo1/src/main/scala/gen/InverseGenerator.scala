package gen

import func.FunctionDefinition
import java.util.Random
import util.Rand

/**
  * Generates realizations of a random variable following the given density function.
  * This implementation is using the inverse of the repartition function to achieve constant time complexity.
  * Based on the answer to question 1.f from TP1.
  */
class InverseGenerator(fd: FunctionDefinition)(implicit random: Random) extends RealizationGenerator[Double] {
	// Random slices generator
	val sg = Rand.sliceGenerator(fd)

	override def produce(): Double = {
		// Select a random slice
		val slice = sg.produce()

		// Inverse if y0 == y1
		def uniformInverse(y: Double) = slice.x0 + y * (slice.x1 - slice.x0)

		// Inverse if y0 != y1
		def affineInverse(y: Double) = {
			val y0_2 = slice.y0 * slice.y0
			val y1_2 = slice.y1 * slice.y1
			slice.x0 + (Math.sqrt(y * (y1_2 - y0_2) + y0_2) - slice.y0) / slice.m
		}

		// Select the correct inverse function
		val inverse = if (slice.y0 == slice.y1) uniformInverse _ else affineInverse _

		// Evaluate it
		inverse(Rand.nextDouble)
	}
}
