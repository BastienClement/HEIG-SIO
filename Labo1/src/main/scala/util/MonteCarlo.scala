package util

/**
  * Perform
  */
object MonteCarlo {
	/** Critical value for a 95% confidence level */
	val z95 = 1.960

	/** Confidence interval */
	case class ConfidenceInterval(min: Double, max: Double) {
		val delta = max - min
	}

	/** Simulation result */
	case class Result(mean: Double, sd: Double, ci: ConfidenceInterval)

	/** Computes statistics for a sequence of realizations */
	def stats(realizations: Seq[Double]): Result = {
		// Sample size
		val n = realizations.length

		// Sample mean
		val mean = realizations.mean

		// Standard deviation estimation
		var s = 0.0
		for (xk <- realizations) s += Math.pow(xk - mean, 2)
		s /= n - 1
		//val s = Math.sqrt(realizations.foldLeft(0.0) { (acc, xk) => acc + Math.pow(xk - mean, 2) / (n - 1) })

		// One half of the confidence interval
		// Using z* instead of t* because it's easier
		val d = z95 * (s / Math.sqrt(n))

		Result(mean, s, ConfidenceInterval(mean - d, mean + d))
	}

	/** Computes n realizations of a random variable and returns the mean and half confidence interval */
	def run(n: Int)(compute: => Double): Result = stats(for (i <- 1 to n) yield compute)

	/** Computes n realizations of a m random variables and returns the mean and half confidence interval */
	def multirun(n: Int)(compute: => Seq[Double]): Seq[Result] = {
		// Generate n realizations
		val results = for (i <- 1 to n) yield compute

		// Transpose seq of realizations of seq of variables
		// to seq of realizations for each variable
		val variables = results.transpose

		// Compute statistics
		variables.map(stats)
	}
}
