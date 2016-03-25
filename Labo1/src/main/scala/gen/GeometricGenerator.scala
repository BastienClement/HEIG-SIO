package gen

import func.FunctionDefinition
import java.util.Random
import util.Rand

/**
  * Generates realizations of a random variable following the given density function.
  * This implementation is using a geometric hit-miss algorithm able to recover
  * from misses and with constant time complexity.
  * Based on the answer to question 2.b from TP1.
  */
class GeometricGenerator(fd: FunctionDefinition)(implicit random: Random) extends RealizationGenerator[Double] {
	// Random slices generator
	val sg = Rand.sliceGenerator(fd)

	override def produce(): Double = {
		// Select a random slice
		val slice = sg.produce()

		// Generate a point (a, b) in the rectangle [x0, x1] x [0, y0 + y1]
		val a = Rand.nextDouble(slice.x0, slice.x1)
		val b = Rand.nextDouble(0, slice.y0 + slice.y1)

		// If (a, b) falls in the function's area return x else use symmetrical point
		if (b <= fd.evaluate(a)) a
		else slice.x1 - (a - slice.x0)
	}
}
