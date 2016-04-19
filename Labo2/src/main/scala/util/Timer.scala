package util

/**
  * Un chronomètre
  */
class Timer {
	/** Nombre de nanosecondes dans une microseconde */
	private final val msFactor = 1000000.0

	/** Temps de départ du chronomètre */
	private var start = now

	/** Temps actuel */
	def now: Long = System.nanoTime()

	/** Nombre de millisecondes depuis le lancement du chronomètre */
	def time: Double = (now - start) / msFactor

	/** Réinitialise le chronomètre */
	def reset(): Double = {
		val t = time
		start = now
		t / msFactor
	}
}
