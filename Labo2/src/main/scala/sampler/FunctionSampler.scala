package sampler

import scala.concurrent.duration.FiniteDuration

/**
  * Échantillonneur de fonction
  */
trait FunctionSampler {
	def sample(a: Double, b: Double): SamplerInstance
}

/**
  * Une instance d'un échantillonneur encapsulant les variables d'état
  * Utilisé pour permettre un échantillonnage itératif jusqu'à atteindre un objectif fixé
  */
trait SamplerInstance {
	/** Nombre d'itérations effectuées */
	def iterations: Long

	/** Intervalle de confiance actuel */
	def interval: Interval

	/**
	  * Execute n itérations
	  *
	  * @param n nombre d'itérations à executer
	  */
	def run(n: Int): SamplerInstance

	/**
	  * Exécute l'échantilonneur pour une durée de temps précise
	  */
	def runFor(d: FiniteDuration, unit: Int = 10000): SamplerInstance = {
		val deadline = d.fromNow
		do run(unit) while (deadline.hasTimeLeft)
		this
	}

	/**
	  * Exécute l'échantilonneur jusqu'à atteindre un intervalle de confiance de la largeur souhaitée
	  */
	def runUntil(delta: Double, unit: Int = 10000): SamplerInstance = {
		do run(unit) while (interval.delta > delta)
		this
	}

	override def toString = s"${this.getClass.getName}\n${interval.toString}"
}

/**
  * Un intervalle de confiance
  *
  * @param mean  moyenne des échantillons
  * @param n     nombre d'échantillons
  * @param sigma écart-type
  */
case class Interval(mean: Double, n: Double, sigma: Double) {
	private final val z95 = 1.960

	val nsigma = sigma * Math.sqrt(n)

	val half = z95 * sigma
	val delta = half * 2
	val min = mean - half
	val max = mean + half

	override def toString = s"$mean ± $half [d=$delta, sigma=$sigma, n=$n, nsigma=$nsigma]\n[$min ; $max] " +
			(if (min < 601.971 && max > 601.971) "OK" else "NOK")
}
