

trait RealizationGenerator {
	def produce(): Double
	def produce(n: Int): Seq[Double] = for (i <- 1 to n) yield produce()
	def stream(): Stream[Double] = produce() #:: stream()
}
