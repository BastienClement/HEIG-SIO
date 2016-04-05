import scala.language.implicitConversions

package object util {
	/**
	  * Computes the mean/stdev of a sequence of Doubles
	  */
	implicit class StatsOps(val seq: Seq[Double]) extends AnyVal {
		@inline def mean = seq.sum / seq.length

		@inline def stdev(mean: Double): Double = {
			val sum = seq.foldLeft(0.0) { (acc, xk) =>
				val dk = xk - mean
				acc + dk * dk
			}
			Math.sqrt(sum / seq.length)
		}

		@inline def stdev: Double = stdev(mean)
	}

	/**
	  * Implicitly converts any type T for which a Numeric[T] exists to a Double
	  */
	@inline implicit def NumericToDouble[T](n: T)(implicit ev: Numeric[T]): Double = ev.toDouble(n)
}
