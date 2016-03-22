import func.FunctionDefinition
import gen.{GeometricGenerator, HitMissGenerator, InverseGenerator}
import scala.util.Random
import util.Timer

object Labo1 extends App {
	implicit val rand = new Random()
	val timer = new Timer

	val ftilde = FunctionDefinition(1.0 -> 2.0, 2.0 -> 5.0, 4.0 -> 3.0, 5.0 -> 3.0)

	val hmg = new HitMissGenerator(ftilde)
	val geg = new GeometricGenerator(ftilde)
	val ing = new InverseGenerator(ftilde)

	val count = 1000000

	def timing(block: => String, length: Int = 7) = {
		timer.reset()
		val res = block
		val time = ((timer.time * 1000).floor / 1000.0).toString.take(length).padTo(length, "0").mkString
		println("(" + time + " ms) " + res)
	}

	timing("Hit-miss average: " + (hmg.produce(count).sum / count))
	timing("Geometric average: " + (geg.produce(count).sum / count))
	timing("Inverse average: " + (ing.produce(count).sum / count))
	timing("Mathematical average: " + ftilde.expectedValue)
}
