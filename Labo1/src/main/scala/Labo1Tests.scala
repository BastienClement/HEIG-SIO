import Labo1._
import func.FunctionDefinition

object Labo1Tests extends App {
	val one = FunctionDefinition(
		5 -> 1,
		15 -> 1
	)


	val two = FunctionDefinition(
		2 -> 0,
		3 -> 1,
		7 -> 0,
		10 -> 0,
		14 -> 1,
		15 -> 0
	)

	Vector(one, two).foreach(benchmarkFunction(_))
}
