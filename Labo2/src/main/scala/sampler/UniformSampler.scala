package sampler
import util.ExtendedRandom

class UniformSampler(val g: Double => Double)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def apply(a: Double, b: Double, n: Int = 10000): Interval = {
		var S = 0.0
		var Q = 0.0

		for (k <- 1 to n) {
			val x = rand.nextDouble(a, b)
			val y = g(x)
			S += y
			Q += y * y
		}

		val Y = S / n
		val sigma2 = (Q / n) - Y * Y

		val G = (b - a) * Y
		val sigma = (b - a) * Math.sqrt(sigma2 / n)

		Interval(G, sigma)
	}
}
