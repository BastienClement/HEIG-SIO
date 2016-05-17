package sampler
import scala.annotation.tailrec
import util.ExtendedRandom

class UniformSampler(val g: Double => Double)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def sample(a: Double, b: Double) = new SamplerInstance {
		var S = 0.0
		var Q = 0.0
		var N = 0L

		def iterations: Long = N

		def interval: Interval = {
			val Y = S / N
			val sigma2 = (Q / N) - Y * Y

			val G = (b - a) * Y
			val sigma = (b - a) * Math.sqrt(sigma2 / N)

			Interval(G, N, sigma)
		}

		@tailrec
		def run(n: Int): SamplerInstance = if (n > 0) {
			val x = rand.nextDouble(a, b)
			val y = g(x)
			S += y
			Q += y * y
			N += 1
			run(n - 1)
		} else {
			this
		}
	}
}
