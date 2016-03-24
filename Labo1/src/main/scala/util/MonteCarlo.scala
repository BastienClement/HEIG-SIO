package util

object MonteCarlo {
	val z95 = 1.960

	def run(n: Int)(compute: => Double) = {
		val res = for (i <- 1 to n) yield compute
		val avg = res.sum / n
		val s = Math.sqrt(res.map { xk => Math.pow(xk - avg, 2) }.sum / (n - 1))
		val d = z95 * (s / Math.sqrt(n))
		(avg, (avg - d, avg + d), d * 2)
	}
}
