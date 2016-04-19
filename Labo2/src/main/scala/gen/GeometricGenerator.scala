package gen

import func.PiecewiseAffineFunction
import util.{DiscreteGenerator, ExtendedRandom}

/**
  * Génère des réalisations d'une variable aléatoire selon la fonction de densité donnée.
  * Cette implémentation utilise une approche acceptation-rejet capable de récupérer un
  * essai manqué pour garantir un temps d'execution constant.
  * Basé sur la réponse à la question 2.b du TP1.
  */
class GeometricGenerator(fd: PiecewiseAffineFunction)(implicit random: ExtendedRandom) extends RealizationGenerator[Double] {
	/** Générateur discret de tranches de la fonction */
	val sg = DiscreteGenerator.ofFunctionSlices(fd)

	override def produce(): Double = {
		// Sélection aléatoire d'une tranche
		val slice = sg.produce()

		// Génère un point (a, b) dans le rectangle [x0, x1] x [0, y0 + y1]
		val a = random.nextDouble(slice.x0, slice.x1)
		val b = random.nextDouble(0, slice.y0 + slice.y1)

		// Si (a, b) est sous la courbe de la fonction, on retourne a directement,
		// sinon on prend le point symétrique pour récupérer un échec.
		if (b <= slice(a)) a
		else slice.x1 - (a - slice.x0)
	}
}
