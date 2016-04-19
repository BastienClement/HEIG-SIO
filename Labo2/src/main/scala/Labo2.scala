import util.MonteCarlo

/**
  * Created by galedric on 18.04.16.
  */
object Labo2 extends App {
	def g(x: Double) = (25 + x * (x - 6) * (x - 8) * (x - 14) / 25) * Math.pow(Math.E, Math.sqrt(1 + Math.cos(x * x / 10)))
	val a = 0
	val b = 15
	val rand = Labo1.defaultRandom(42)
	val points = 1000000

	val G = MonteCarlo.run(100) {
		val avg = Stream.continually(g(rand.nextDouble(a, b))).take(points).sum / points
		avg * (b - a)
	}

	println(G)
	println(G.ci.max - G.ci.min)
}
