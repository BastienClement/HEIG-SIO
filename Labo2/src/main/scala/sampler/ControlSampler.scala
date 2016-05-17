package sampler

import scala.annotation.tailrec
import util.{ExtendedRandom, FunctionSlicer, StatsOps}

/**
  * Échantillonneur uniforme avec variable de contrôle
  *
  * @param g      la fonction à intégrer
  * @param slices le nombre de morceaux à construire pour la fonction auxilière h(x)
  * @param m      le nombre d'itération a effectuer pour estimer la constante c
  * @param rand   un générateur de nombre aléatoire
  */
class ControlSampler(g: Double => Double, slices: Int = 15, m: Int = 10000)(implicit val rand: ExtendedRandom) extends FunctionSampler {
	def sample(a: Double, b: Double) = new SamplerInstance {
		/** Fonction auxilière h(x) */
		val h = FunctionSlicer.slice(g, a, b, slices)
		val mu = h.area / (b - a)

		/** Calcul de c */
		val (mY, mZ) = Stream.continually(rand.nextDouble(a, b)).take(m).map(x => (g(x), h(x))).toVector.unzip

		val VarZ = mZ.variance
		val Y = mY.mean

		val CovYZ = (0 until m).map(k => (mY(k) - Y) * (mZ(k) - mu)).sum / m
		val c = -CovYZ / VarZ

		/** Accumulateurs */
		val Vks = (0 until m).map(k => mY(k) + c * (mZ(k) - mu))
		var Sv = Vks.sum
		var Qv = Vks.map(vk => vk * vk).sum

		/** Nombre d'itérations */
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
			val vk = yk + c * (zk - mu)
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
