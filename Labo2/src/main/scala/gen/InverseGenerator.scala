package gen

import func.PiecewiseAffineFunction
import util.{DiscreteGenerator, ExtendedRandom}

/**
  * Génère des réalisations d'une variable aléatoire selon la fonction de densité donnée.
  * Cette implémentation utilise l'inverse de la fonction de répartition pour générer des
  * réalisation en temps constant.
  * Basé sur les réponses à la question 1.e du TP1.
  */
class InverseGenerator(fd: PiecewiseAffineFunction)(implicit random: ExtendedRandom) extends RealizationGenerator[Double] {
	/** Générateur discret de tranches de la fonction */
	val sg = DiscreteGenerator.ofFunctionSlices(fd)

	override def produce(): Double = {
		// Sélection aléatoire d'une tranche
		val slice = sg.produce()

		// Sélection d'une valeur de y à inverser
		val y = random.nextDouble()

		if (slice.y0 == slice.y1) {
			slice.x0 + y * (slice.x1 - slice.x0)
		} else {
			val y0_2 = slice.y0 * slice.y0
			val y1_2 = slice.y1 * slice.y1
			slice.x0 + (Math.sqrt(y * (y1_2 - y0_2) + y0_2) - slice.y0) / slice.m
		}
	}
}
