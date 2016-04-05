import func.FunctionDefinition
import func.FunctionDefinition.Slice
import gen.{GeometricGenerator, HitMissGenerator, InverseGenerator}
import java.util.Random
import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import util.{ExtendedRandom, MonteCarlo, StatsOps, Timer}

object Labo1 extends App {
	/**
	  * Constructs a default random generator
	  */
	def defaultRandom = new Random(42) with ExtendedRandom

	/**
	  * Computes the ratio between the function's area and its bounding box
	  */
	def areaRatio(fd: FunctionDefinition): Double = fd.area / (fd.ym * (fd.b - fd.a))

	/**
	  * Crafts a random function definition according to given parameters
	  */
	@tailrec
	def craftFunction(n: Int, ymin: Int, ymax: Int, a: Int, b: Int)
	                 (implicit random: ExtendedRandom): FunctionDefinition = {
		if (n < 2 || ymax == 0 || a >= b) throw new IllegalArgumentException

		val xs = (1 to (n * 2)).map(_ => random.nextInt(a, b)).distinct.take(n)
		lazy val points = xs.sorted.map(x => (x, random.nextInt(ymin, ymax + 1)): (Double, Double))
		lazy val valid = !points.forall { case (_, y) => y == 0 }

		if (xs.length == n && valid) FunctionDefinition(points)
		else craftFunction(n, ymin, ymax, a, b)
	}

	/**
	  * Crafts a random function definition according to given parameters and
	  * checks that its area is within given bounds
	  */
	@tailrec
	def craftFunctionWithRatio(n: Int, ymin: Int, ymax: Int, a: Int, b: Int, rmin: Double, rmax: Double)
	                          (implicit random: ExtendedRandom): FunctionDefinition = {
		val fd = craftFunction(n, ymin, ymax, a, b)
		val ratio = areaRatio(fd)

		if (ratio >= rmin && ratio <= rmax) fd
		else craftFunctionWithRatio(n, ymin, ymax, a, b, rmin, rmax)
	}

	/**
	  * Crafts a random function with good params
	  */
	def craftSuitableFunction(ratio: Double): FunctionDefinition = {
		implicit val rand = new Random() with ExtendedRandom
		craftFunctionWithRatio(8, 2, 8, 1, 9, ratio - 0.05, ratio + 0.05)
	}

	// Generator factories
	def hmg(fd: FunctionDefinition) = {
		implicit val rand = defaultRandom
		new HitMissGenerator(fd)
	}

	def geg(fd: FunctionDefinition) = {
		implicit val rand = defaultRandom
		new GeometricGenerator(fd)
	}

	def ing(fd: FunctionDefinition) = {
		implicit val rand = defaultRandom
		new InverseGenerator(fd)
	}

	/**
	  * Generates a lot a random values to trigger JIT compilation
	  */
	def warmup() = {
		println("Warming up...\n")

		val count = 1000000
		val fd = craftSuitableFunction(0.5)

		for (i <- 1 to 100) {
			hmg(fd).produce(count)
			geg(fd).produce(count)
			ing(fd).produce(count)
		}
	}

	def benchmarkFunction(fd: FunctionDefinition, runs: Int = 10000, count: Int = 100000, parallel: Boolean = true) = {
		println("\nTesting function:")

		// Prints points
		val points = fd.slices.flatMap { case Slice(x0, y0, x1, y1) => Array((x0, y0), (x1, y1)) }.distinct
		println(points.mkString(", "))

		println(s"\nSampling: $runs x $count")

		val mev = fd.expectedValue
		val ratio = areaRatio(fd)

		// Mathematical expected value and ratio
		println("\nMathematical expected value: " + mev)
		println("Function area ratio: " + ratio + "\n")

		// Benchmark generators
		val timer = new Timer
		val generators = Seq(hmg _, geg _, ing _)
		val res = for (gen_factory <- if (parallel) generators.par else generators) yield {
			implicit val rand = ThreadLocalRandom.current()
			val gen = gen_factory(fd)
			val timer = new Timer
			val name = gen.getClass.getSimpleName

			val Seq(ev, time) = MonteCarlo.multirun(runs) {
				timer.reset()

				val realizations = gen.produce(count)

				val time = timer.time
				val ev = realizations.mean

				Vector(ev, time)
			}

			val res = if (mev >= ev.ci.min && mev <= ev.ci.max) "OK" else "NOK"

			s"""$name: $res
				|         Stats |         Mean |      Std Dev |             CI              |     CI Delta
				|-----------------------------------------------------------------------------------------
				|Expected value | %12f | %12f | [%12f;%12f] | %12f
				|     Time [ms] | %12f | %12f | [%12f;%12f] | %12f
				""".stripMargin.format(
					ev.mean, ev.sd, ev.ci.min, ev.ci.max, ev.ci.delta,
					time.mean, time.sd, time.ci.min, time.ci.max, time.ci.delta
			)
		}

		// Print results as CSV
		res.seq.foreach(println _)
		println("\nTime: " + timer.time / 1000.0 + " seconds")
	}


	warmup()
	Seq(0.4, 0.6, 0.9).map(craftSuitableFunction).foreach(benchmarkFunction(_))
}
