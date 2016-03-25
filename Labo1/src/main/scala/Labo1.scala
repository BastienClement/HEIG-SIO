import func.FunctionDefinition
import func.FunctionDefinition.Slice
import gen.{GeometricGenerator, HitMissGenerator, InverseGenerator}
import java.util.Random
import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import util.{MonteCarlo, Rand, StatsOps, Timer}

object Labo1 extends App {
	/**
	  * Constructs a default random generator
	  */
	def defaultRandom = ThreadLocalRandom.current()

	/**
	  * Computes the ratio between the function's area and its bounding box
	  */
	def areaRatio(fd: FunctionDefinition): Double = fd.area / (fd.ym * (fd.b - fd.a))

	/**
	  * Crafts a random function definition according to given parameters
	  */
	@tailrec
	def craftFunction(n: Int, ymin: Int, ymax: Int, a: Int, b: Int)
	                 (implicit random: Random): FunctionDefinition = {
		if (n < 2 || ymax == 0 || a >= b) throw new IllegalArgumentException

		val xs = (1 to (n * 2)).map(_ => Rand.nextInt(a, b)).distinct.take(n)
		lazy val points = xs.sorted.map(x => (x, Rand.nextInt(ymin, ymax + 1)): (Double, Double))
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
	                          (implicit random: Random): FunctionDefinition = {
		val fd = craftFunction(n, ymin, ymax, a, b)
		val ratio = areaRatio(fd)

		if (ratio >= rmin && ratio <= rmax) fd
		else craftFunctionWithRatio(n, ymin, ymax, a, b, rmin, rmax)
	}

	/**
	  * Crafts a random function with good params
	  */
	def craftSuitableFunction(ratio: Double): FunctionDefinition = {
		implicit val rand = new Random()
		craftFunctionWithRatio(8, 2, 8, 1, 9, ratio - 0.05, ratio + 0.05)
	}

	// Generator factories
	def hmg(fd: FunctionDefinition) = {
		implicit val rand = defaultRandom; new HitMissGenerator(fd)
	}

	def geg(fd: FunctionDefinition) = {
		implicit val rand = defaultRandom; new GeometricGenerator(fd)
	}

	def ing(fd: FunctionDefinition) = {
		implicit val rand = defaultRandom; new InverseGenerator(fd)
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

	warmup()

	for (target_ratio <- Seq(0.4, 0.6, 0.9)) {
		// The function for this run
		val fd = craftSuitableFunction(target_ratio)
		val ratio = areaRatio(fd)

		// Mathematical expected value and ratio
		println("\nMathematical expected value: " + fd.expectedValue)
		println("Function area ratio: " + ratio + "\n")

		// Prints points
		val points = fd.slices.flatMap { case Slice(x0, y0, x1, y1) => Array((x0, y0), (x1, y1)) }.distinct
		println(points.mkString(", "))
		println()

		// Benchmark generators
		val timer = new Timer
		val res = for (gen_factory <- Seq(hmg _, geg _, ing _).par) yield {
			implicit val rand = ThreadLocalRandom.current()
			val gen = gen_factory(fd)
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

			s"$name,$ratio,${ev.mean},${ev.sd},${ev.ci.min},${ev.ci.max},${ev.ci.delta}," +
					s"${time.mean},${time.sd},${time.ci.min},${time.ci.max},${time.ci.delta}"
		}

		// Print results as CSV
		res.seq.foreach(println _)
	}
}
