package sampler

import gen.InverseGenerator
import util.{ExtendedRandom, FunctionSlicer}

class ImportanceSampler(val g: Double => Double, slices: Int)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def apply(a: Double, b: Double, n: Int = 10000): Interval = {
		val f = FunctionSlicer.slice(g, a, b, slices).proportionalDensity
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
		val sigma2 = (Q / n) - (Y * Y)

		val G = Y
		val sigma = Math.sqrt(sigma2 / n)

		Interval(G, sigma)
	}
}
