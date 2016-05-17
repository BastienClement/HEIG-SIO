package sampler

import scala.concurrent.duration.FiniteDuration

trait FunctionSampler {
	def sample(a: Double, b: Double): SamplerInstance
}

trait SamplerInstance {
	def iterations: Long
	def interval: Interval
	def run(n: Int): SamplerInstance

	def runFor(d: FiniteDuration, unit: Int = 10000): SamplerInstance = {
		val deadline = d.fromNow
		do run(unit) while (deadline.hasTimeLeft)
		this
	}

	def runUntil(delta: Double, unit: Int = 10000): SamplerInstance = {
		do run(unit) while (interval.delta > delta)
		this
	}

	override def toString = s"${this.getClass.getName}\n${interval.toString}"
}

case class Interval(mean: Double, n: Double, sigma: Double) {
	private final val z95 = 1.960

	val nsigma = sigma * Math.sqrt(n)

	val half = z95 * sigma
	val delta = half * 2
	val min = mean - half
	val max = mean + half

	override def toString = s"$mean ± $half [d=$delta, n=$n, nsigma=$nsigma]\n[$min ; $max] " +
			(if (min < 601.971 && max > 601.971) "OK" else "NOK")
}
