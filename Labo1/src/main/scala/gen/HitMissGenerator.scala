package gen

import func.FunctionDefinition
import scala.annotation.tailrec
import scala.util.Random
import util.Rand

class HitMissGenerator(fd: FunctionDefinition)(implicit random: Random) extends RealizationGenerator[Double] {
	@tailrec
	final override def produce(): Double = {
		val x = Rand.nextDouble(fd.a, fd.b)
		val y = Rand.nextDouble(0, fd.ym)
		if (y <= fd.evaluate(x)) x else produce()
	}
}
