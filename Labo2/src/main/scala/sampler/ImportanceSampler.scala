package sampler

import gen.InverseGenerator
import scala.annotation.tailrec
import util.{ExtendedRandom, FunctionSlicer}

class ImportanceSampler(val g: Double => Double, slices: Int = 15)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def sample(a: Double, b: Double) = new SamplerInstance {
		val f = FunctionSlicer.slice(g, a, b, slices).proportionalDensity
		val gen = new InverseGenerator(f)

		var S = 0.0
		var Q = 0.0
		var N = 0L

		def iterations: Long = N

		def interval: Interval = {
			val Y = S / N
			val sigma2 = (Q / N) - (Y * Y)

			val G = Y
			val sigma = Math.sqrt(sigma2 / N)

			Interval(G, N, sigma)
		}

		@tailrec
		def run(n: Int): SamplerInstance = if (n > 0) {
			val x = gen.produce()
			val y = g(x) / f(x)
			S += y
			Q += y * y
			N += 1
			run(n - 1)
		} else {
			this
		}
	}
}
