package gen

trait RealizationGenerator[+T] {
	def produce(): T
	def produce(n: Int): Seq[T] = for (i <- 1 to n) yield produce()
	def stream(): Stream[T] = produce() #:: stream()
}
