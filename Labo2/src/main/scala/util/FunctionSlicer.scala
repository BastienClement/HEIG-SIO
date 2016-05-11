package util

import func.PiecewiseAffineFunction

object FunctionSlicer {
	def slice(g: Double => Double, a: Double, b: Double, slices: Int) = {
		val step = (b - a) / slices
		val points = Stream.iterate(a)(i => i + step).take(slices + 1).map(x => (x, g(x)))
		PiecewiseAffineFunction(points)
	}
}
