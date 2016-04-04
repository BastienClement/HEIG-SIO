package gen

import func.FunctionDefinition
import java.util.Random
import util.{DiscreteGenerator, ExtendedRandom}

/**
  * Generates realizations of a random variable following the given density function.
  * This implementation is using a geometric hit-miss algorithm able to recover
  * from misses and with constant time complexity.
  * Based on the answer to question 2.b from TP1.
  */
class GeometricGenerator(fd: FunctionDefinition)(implicit random: ExtendedRandom) extends RealizationGenerator[Double] {
	// Random slices generator
	val sg = DiscreteGenerator.ofFunctionSlices(fd)

	override def produce(): Double = {
		// Select a random slice
		val slice = sg.produce()

		// Generate a point (a, b) in the rectangle [x0, x1] x [0, y0 + y1]
		val a = random.nextDouble(slice.x0, slice.x1)
		val b = random.nextDouble(0, slice.y0 + slice.y1)

		// If (a, b) falls in the function's area return x else use symmetrical point
		// There is no point in evaluating the whole function since the x value will
		// fall in the slice we already have. Simply evaluate the slice.
		if (b <= slice.evaluate(a)) a
		else slice.x1 - (a - slice.x0)
	}
}
