import scala.util.Random

object Labo1 extends App {
	implicit val rand = new Random()
	val timer = new Timer

	val ftilde = FunctionDefinition(-5.0 -> 10.0, 1.0 -> 2.0, 2.0 -> 5.0, 4.0 -> 3.0, 5.0 -> 3.0)

	val hmg = new HitMissGenerator(ftilde)
}
