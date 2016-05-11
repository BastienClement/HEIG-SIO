package sampler

/**
  * Created by galedric on 10.05.16.
  */
case class Interval(mean: Double, sigma: Double) {
	private final val z95 = 1.960

	val half = z95 * sigma
	val min = mean - half
	val max = mean + half

	override def toString = s"$mean ± $half\n[$min ; $max]"
}
