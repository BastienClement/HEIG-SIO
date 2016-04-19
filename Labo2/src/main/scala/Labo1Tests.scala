import Labo1._
import func.FunctionDefinition

/**
  * Tests des fonctions données en exemple
  */
object Labo1Tests extends App {
	/** Uniforme */
	val one = FunctionDefinition(
		5 -> 1,
		15 -> 1
	)

	/** Double triangulaire */
	val two = FunctionDefinition(
		2 -> 0,
		3 -> 1,
		7 -> 0,
		10 -> 0,
		14 -> 1,
		15 -> 0
	)

	/** Profil plat */
	val three = FunctionDefinition(
		2 -> 8,
		4 -> 10,
		7 -> 10,
		9 -> 9,
		12 -> 5,
		13 -> 9,
		17 -> 10,
		20 -> 6
	)

	/** Profil accidenté */
	val four = FunctionDefinition(
		2 -> 1,
		3 -> 10,
		5 -> 0,
		10 -> 1,
		12 -> 8,
		13 -> 4,
		15 -> 1,
		17 -> 0,
		19 -> 2,
		20 -> 9
	)

	def argument(idx: Int, default: Int) =
		if (args.length >= idx + 1) Integer.valueOf(args(idx)).intValue()
		else default

	val iter = argument(0, 10000)
	val count = argument(1, 1000000)
	val parallel = argument(2, 1) == 1
	val seed = argument(3, 42)

	warmup()
	Vector(one, two, three, four).foreach(fn => benchmarkFunction(fn, iter, count, parallel, seed))
}
