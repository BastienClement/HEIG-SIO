package sampler

import func.PiecewiseAffineFunction
import gen.InverseGenerator
import util.ExtendedRandom

class ImportanceSampler(val g: Double => Double, slices: Int)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def apply(a: Double, b: Double, n: Int = 10000): Interval = {
		val step = (b - a) / slices
		val points = Stream.iterate(a)(i => i + step).take(slices + 1).map(x => (x, g(x)))
		val f = PiecewiseAffineFunction(points).proportionalDensity
		val gen = new InverseGenerator(f)

		var S = 0.0
		var Q = 0.0

		for (k <- 1 to n) {
			val x = gen.produce()
			val y = g(x) / f(x)
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
