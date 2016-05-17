package sampler

import scala.annotation.tailrec
import util.{ExtendedRandom, FunctionSlicer, StatsOps}

class ControlSampler(g: Double => Double, slices: Int = 15, m: Int = 10000)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def sample(a: Double, b: Double) = new SamplerInstance {
		val h = FunctionSlicer.slice(g, a, b, slices)
		val mu = h.area / (b-a)

		val (mY, mZ) = Stream.continually(rand.nextDouble(a, b)).take(m).map(x => (g(x), h(x))).toVector.unzip

		val VarZ = mZ.variance
		val Y = mY.mean

		val CovYZ = (0 until m).map(k => (mY(k) - Y) * (mZ(k) - mu)).sum / m
		val c = -CovYZ / VarZ

		val Vks = (0 until m).map(k => mY(k) + c * (mZ(k) - mu))
		var Sv = Vks.sum
		var Qv = Vks.map(vk => vk * vk).sum

		var N: Long = m

		def iterations: Long = N

		def interval: Interval = {
			val V = Sv / N
			val sigma2 = (Qv / N) - (V * V)
			val G = (b - a) * V
			val sG = (b - a) * Math.sqrt(sigma2 / N)
			Interval(G, N, sG)
		}

		@tailrec
		def run(n: Int): SamplerInstance = if (n > 0) {
			val xk = rand.nextDouble(a, b)
			val yk = g(xk)
			val zk = h(xk)
			val vk = yk + c * (zk  - mu)
			Sv += vk
			Qv += vk * vk
			N += 1
			run(n - 1)
		} else {
			this
		}

		override def toString = s"${super.toString} [c=$c]"
	}
}
