import scala.util.Random

object Labo1 extends App {
	implicit val rand = new Random()
	val timer = new Timer

	val ftilde = FunctionDefinition(1.0 -> 2.0, 2.0 -> 5.0, 4.0 -> 3.0, 5.0 -> 3.0)

	val sg = Rand.sliceGenerator(ftilde)
	println(sg.produce(10))

	val hmg = new HitMissGenerator(ftilde)
}
