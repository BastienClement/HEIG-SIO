package sampler

import util.{ExtendedRandom, FunctionSlicer, StatsOps}

class ControlSampler(g: Double => Double, slices: Int, m: Int)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def apply(a: Double, b: Double, n: Int): Interval = {
		val h = FunctionSlicer.slice(g, a, b, slices)
		val mu = h.expectedValue

		val (mY, mZ) = Stream.continually(rand.nextDouble(a, b)).take(m).map(x => (g(x), h(x))).toVector.unzip

		val VarZ = mZ.variance
		val Y = mY.mean

		val CovYZ = (0 until m).map(k => (mY(k) - Y) * (mZ(k) - mu)).sum / m
		val c = -CovYZ / VarZ

		val Vks = (0 until m).map(k => mY(k) + c * (mZ(k) - mu))
		var Sv = Vks.sum
		var Qv = Vks.map(vk => vk * vk).sum

		for (_ <- m until n) {
			val xk = rand.nextDouble(a, b)
			val yk = g(xk)
			val zk = h(xk)
			val vk = yk + c * (zk  - mu)
			Sv += vk
			Qv += vk * vk
		}

		val V = Sv / n
		val sigma2 = (Qv / n) - (V * V)
		val G = (b - a) * V
		val sG = (b - a) * Math.sqrt(sigma2 / n)

		Interval(G, sG)
	}
}
