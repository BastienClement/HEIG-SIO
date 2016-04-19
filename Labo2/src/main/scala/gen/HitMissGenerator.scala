package gen

import func.FunctionDefinition
import scala.annotation.tailrec
import util.ExtendedRandom

/**
  * Génère des réalisations d'une variable aléatoire selon la fonction de densité donnée.
  * Cette implémentation utilise une approche acceptation-rejet avec un temps d'execution non-borné.
  * Basé sur la réponse à la question 2.a du TP1.
  */
class HitMissGenerator(fd: FunctionDefinition)(implicit random: ExtendedRandom) extends RealizationGenerator[Double] {
	@tailrec final override def produce(): Double = {
		// Génère un point (x, y) dans le rectangle encadrant la fonction
		val x = random.nextDouble(fd.a, fd.b)
		val y = random.nextDouble(0, fd.ym)

		// Si le point tombe sous la courbe de la fonction, la valeur x est retournée,
		// sinon un autre point est généré jusqu'à ce que la condition soit vérifiée,
		if (y < fd.evaluate(x)) x else produce()
	}
}
