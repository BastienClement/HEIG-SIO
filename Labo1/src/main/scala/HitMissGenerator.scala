import scala.annotation.tailrec
import scala.util.Random

class HitMissGenerator(f: FunctionDefinition)(implicit random: Random) extends RealizationGenerator[Double] {
	@tailrec
	final override def produce(): Double = {
		val x = Rand.nextDouble(f.a, f.b)
		val y = Rand.nextDouble(0, f.ym)
		if (y <= f.evaluate(x)) x else produce()
	}
}
