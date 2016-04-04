package gen

import func.FunctionDefinition
import scala.annotation.tailrec
import java.util.Random
import util.ExtendedRandom

/**
  * Generates realizations of a random variable following the given density function.
  * This implementation is using a simple geometric hit-miss algorithm with unbound theoretical time-complexity.
  */
class HitMissGenerator(fd: FunctionDefinition)(implicit random: ExtendedRandom) extends RealizationGenerator[Double] {
	@tailrec final override def produce(): Double = {
		// Generate a random (x, y) point in the bounding box around the function
		val x = random.nextDouble(fd.a, fd.b)
		val y = random.nextDouble(0, fd.ym)

		// If the point falls inside the function's area the x-value is returned
		// else another point is generated until the condition is verified.
		if (y <= fd.evaluate(x)) x else produce()
	}
}
