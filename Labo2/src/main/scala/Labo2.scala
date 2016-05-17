import java.util.Random
import sampler._
import util.{ExtendedRandom, Timer}
import scala.concurrent.duration._

/**
  * Laboratoire 2
  */
object Labo2 extends App {
	/** La fonction à intégrer */
	def g(x: Double): Double =
		(25 + x * (x - 6) * (x - 8) * (x - 14) / 25) * Math.pow(Math.E, Math.sqrt(1 + Math.cos(x * x / 10)))

	/** Bornes d'intégration */
	val (a, b) = (0, 15)

	/** Initialisation de l'aléatoire */
	def defaultRandom = new Random(2016) with ExtendedRandom
	def init(block: ExtendedRandom => FunctionSampler) = block(defaultRandom)

	/** Liste des samplers */
	val samplers = Seq(
		init { implicit rand => new UniformSampler(g) },
		init { implicit rand => new ImportanceSampler(g) },
		init { implicit rand => new ControlSampler(g) }
	).par

	/** Démarrage... */
	println("Warming up...")
	samplers.foreach(s => s.sample(a, b).run(100000000)) // 100M

	/** 10 min */
	println("\nRunning for 10 minutes...")
	println(samplers.map(implicit s => s.sample(a, b).runFor(10.minutes).toString).mkString("\n\n"))

	/** Target */
	for (target <- Seq(0.5, 0.25, 0.125, 0.05, 0.025)) {
		println(s"\nTargeting d = $target...")

		println(samplers.par.map { sampler =>
			val timer = new Timer
			val res = sampler.sample(a, b).runUntil(target)
			s"${res.toString} [" + (timer.time / 1000) + " sec]"
		}.mkString("\n\n"))
	}
}
