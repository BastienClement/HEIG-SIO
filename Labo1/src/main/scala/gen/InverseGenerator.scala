package gen

import func.FunctionDefinition
import scala.util.Random
import util.Rand

class InverseGenerator(fd: FunctionDefinition)(implicit random: Random) extends RealizationGenerator[Double] {
	val sg = Rand.sliceGenerator(fd)

	override def produce(): Double = {
		val slice = sg.produce()

		// Inverse if y0 == y1
		def uniformInverse(y: Double) = slice.x0 + y * (slice.x1 - slice.x0)

		// Inverse if y0 != y1
		def affineInverse(y: Double) = {
			val y0_2 = slice.y0 * slice.y0
			val y1_2 = slice.y1 * slice.y1
			slice.x0 + (Math.sqrt(y * (y1_2 - y0_2) + y0_2) - slice.y0) / slice.m
		}

		val inverse = if (slice.y0 == slice.y1) uniformInverse _ else affineInverse _
		inverse(Rand.nextDouble)
	}
}
