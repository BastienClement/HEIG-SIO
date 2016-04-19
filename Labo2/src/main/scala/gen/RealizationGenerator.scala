package gen

/**
  * Un générateur de réalisation d'une variable aléatoire.
  * @tparam T le type de la variable aléatoire générée
  */
trait RealizationGenerator[+T] {
	/** Produit une réalisation de la variable aléatoire */
	def produce(): T

	/** Produit plusieurs réalisations de la variable aléatoire */
	def produce(n: Int): Seq[T] = for (i <- 1 to n) yield produce()

	/** Produit un flux infini de réalisations de la variable aléatoire */
	def stream(): Stream[T] = produce() #:: stream()
}
