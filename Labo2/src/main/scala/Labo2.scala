import java.util.Random
import sampler.ControlSampler
import util.{ExtendedRandom, Timer}

/**
  * Created by galedric on 18.04.16.
  */
object Labo2 extends App {
	def g(x: Double) = (25 + x * (x - 6) * (x - 8) * (x - 14) / 25) * Math.pow(Math.E, Math.sqrt(1 + Math.cos(x * x / 10)))

	val a = 0
	val b = 15

	//implicit val rand = Labo1.defaultRandom(42)
	implicit val rand = new Random(2016) with ExtendedRandom
	val n = 100000000

	val timer = new Timer
	val sampler = new ControlSampler(g, 10, 10000)
	println(sampler(a, b, n))
	println(timer.time / 1000 + " sec")
}
