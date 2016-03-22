package gen

import func.FunctionDefinition
import scala.util.Random
import util.Rand

class GeometricGenerator(fd: FunctionDefinition)(implicit random: Random) extends RealizationGenerator[Double] {
	val sg = Rand.sliceGenerator(fd)

	override def produce(): Double = {
		val slice = sg.produce()

		val a = Rand.nextDouble(slice.x0, slice.x1)
		val b = Rand.nextDouble(0, slice.y0 + slice.y1)

		if (b <= fd.evaluate(a)) a
		else slice.x1 - (a - slice.x0)
	}
}
