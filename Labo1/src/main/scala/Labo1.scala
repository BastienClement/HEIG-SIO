import scala.util.Random

object Labo1 extends App {
	implicit val rand = new Random()
	val timer = new Timer

	val ftilde = FunctionDefinition(1.0 -> 2.0, 2.0 -> 5.0, 4.0 -> 3.0, 5.0 -> 3.0)

	val ev = ftilde.expectedValue
	val hmg = new HitMissGenerator(ftilde)

	val count = 1000000
	val exp = hmg.produce(count).sum / count

	println("Experimental: " + exp)
	println("Expected value: " + ev)
}
