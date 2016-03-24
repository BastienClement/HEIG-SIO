import func.FunctionDefinition
import gen.{GeometricGenerator, HitMissGenerator, InverseGenerator}
import scala.util.Random
import util.{MonteCarlo, Timer}

object Labo1 extends App {
	implicit val rand = new Random()
	val timer = new Timer

	val ftilde = FunctionDefinition(1.0 -> 2.0, 2.0 -> 5.0, 4.0 -> 3.0, 5.0 -> 3.0)

	val hmg = new HitMissGenerator(ftilde)
	val geg = new GeometricGenerator(ftilde)
	val ing = new InverseGenerator(ftilde)

	val count = 1000000

	def timing[T, U](prefix: String, gen: => T, formatter: T => U, length: Int = 7) = {
		timer.reset()
		val res = gen
		val time = timer.time.toString.take(length).padTo(length, "0").mkString
		println("(" + time + " ms) " + prefix + ": " + formatter(res).toString)
	}

	def average(seq: Seq[Double]) = seq.sum / count

	timing("Hit-miss average", hmg.produce(count), average)
	timing("Geometric average", geg.produce(count), average)
	timing("Inverse average", ing.produce(count), average)
	timing("Mathematical average", ftilde.expectedValue, identity[Double])

	println("\nWarming up...\n")
	for (i <- 1 to 30) {
		hmg.produce(count)
		geg.produce(count)
		ing.produce(count)
	}

	timing("Hit-miss average", hmg.produce(count), average)
	timing("Geometric average", geg.produce(count), average)
	timing("Inverse average", ing.produce(count), average)
	timing("Mathematical average", ftilde.expectedValue, identity[Double])

	val mc = MonteCarlo.run(100) {
		val timer = new Timer
		val count = 1000000
		hmg.produce(count)
		timer.time
	}

	println(mc)
}
