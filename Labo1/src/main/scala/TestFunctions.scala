import func.FunctionDefinition

object TestFunctions {
	val one = FunctionDefinition(5 -> 1, 15 -> 1)
	val two = FunctionDefinition(
		2 -> 0,
		3 -> 1,
		7 -> 0,
		10 -> 0,
		14 -> 1,
		15 -> 0
	)
}
