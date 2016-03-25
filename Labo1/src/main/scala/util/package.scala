package object util {
	/**
	  * Computes the mean of a sequence of Doubles
	  */
	implicit class StatsOps(val seq: Seq[Double]) extends AnyVal {
		@inline def mean = seq.sum / seq.length

		@inline def stdev = {
			val sum = seq.foldLeft(0.0) { (acc, xk) =>
				val dk = xk - mean
				acc + dk * dk
			}
			Math.sqrt(sum / (seq.length - 1))
		}
	}
}
