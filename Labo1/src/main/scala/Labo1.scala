import func.FunctionDefinition
import gen.{GeometricGenerator, HitMissGenerator, InverseGenerator}
import java.util.concurrent.ThreadLocalRandom
import util.{MonteCarlo, Timer, StatsOps}

object Labo1 extends App {
	// Function definition
	val f = FunctionDefinition(
		1.0 -> 2.0,
		2.0 -> 5.0,
		4.0 -> 3.0,
		5.0 -> 3.0,
		6.0 -> 4.0,
		8.0 -> 6.0,
		9.0 -> 7.0
	)

	// Hit-Miss generator factory
	def hmg = {
		implicit val rand = ThreadLocalRandom.current()
		new HitMissGenerator(f)
	}

	// Geometric generator factory
	def geg = {
		implicit val rand = ThreadLocalRandom.current()
		new GeometricGenerator(f)
	}

	// Inverse function generator factory
	def ing = {
		implicit val rand = ThreadLocalRandom.current()
		new InverseGenerator(f)
	}

	// Generate a lot a random values to trigger JIT compilation
	println("Warming up...\n")
	val warming_count = 1000000
	for (i <- 1 to 100) {
		hmg.produce(warming_count)
		geg.produce(warming_count)
		ing.produce(warming_count)
	}

	// Mathematical expected value
	println("Mathematical expected value: " + f.expectedValue + "\n")

	// Benchmark generators
	val timer = new Timer
	val res = for (gen_factory <- Seq(hmg _, geg _, ing _).par) yield {
		val gen = gen_factory()
		val timer = new Timer
		val name = gen.getClass.getSimpleName

		val Seq(ev, time) = MonteCarlo.multirun(10000) {
			val count = 100000
			timer.reset()

			val realizations = gen.produce(count)

			val time = timer.time
			val ev = realizations.mean

			Vector(ev, time)
		}

		s"$name,EV,${ev.mean},${ev.sd},${ev.ci.min},${ev.ci.max},${ev.ci.delta}," +
				s"T,${time.mean},${time.sd},${time.ci.min},${time.ci.max},${time.ci.delta}"
	}

	// Print results as CSV
	res.seq.foreach(println _)
	println(timer.time)
}
