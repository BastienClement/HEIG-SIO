package gen

/**
  * A random variable realization generator.
  * @tparam T the type of the random variable being generated
  */
trait RealizationGenerator[+T] {
	/**
	  * Produces a single realization of the random variable.
	  */
	def produce(): T

	/**
	  * Produces n realizations of the random variable.
	  */
	def produce(n: Int): Seq[T] = for (i <- 1 to n) yield produce()

	/**
	  * Produces an infinite stream of realizations of the random variable.
	  */
	def stream(): Stream[T] = produce() #:: stream()
}
