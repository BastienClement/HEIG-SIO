package sampler

case class Interval(mean: Double, sigma: Double) {
	private final val z95 = 1.960

	val half = z95 * sigma
	val min = mean - half
	val max = mean + half

	override def toString = s"$mean ± $half\n[$min ; $max]"
}

trait FunctionSampler {
	def apply(a: Double, b: Double, iter: Int): Interval
}
