import FunctionDefinition.Slice
import scala.annotation.tailrec

object FunctionDefinition {
	def apply(points: (Double, Double)*) = new FunctionDefinition(slicesFromPoints(points))
	def apply(points: IndexedSeq[(Double, Double)]) = new FunctionDefinition(slicesFromPoints(points))
	def apply(slices: Iterable[Slice]) = new FunctionDefinition(slices.toIndexedSeq)

	/** Constructs an indexed sequence of Slices from a sequence of points (given as pairs) */
	def slicesFromPoints(points: Seq[(Double, Double)]): IndexedSeq[Slice] = {
		points.sliding(2).map { case Seq((x0, y0), (x1, y1)) => Slice(x0, y0, x1, y1) }.toIndexedSeq
	}

	/** A slice of the function. */
	case class Slice(x0: Double, y0: Double, x1: Double, y1: Double) {
		if (x0 >= x1) throw new IllegalArgumentException(s"x0 [$x0] must be less than x1 [$x1]")
		if (y0 < 0 || y1 < 0) throw new IllegalArgumentException(s"y0 [$y0] and y1 [$y1] must be greater or equal to 0")

		val m = (y1 - y0) / (x1 - x0)
		val area = (x1 - x0) * (y1 + y0) / 2
		val expectedValue = (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1)) / (3 * (y0 + y1))

		def contains(x: Double) = x >= x0 && x <= x1
		def evaluate(x: Double) = m * (x - x0) + y0
	}
}

class FunctionDefinition(val slices: IndexedSeq[Slice]) {
	// Check that we have at least two points
	if (slices.length < 2) {
		throw new IllegalArgumentException("Function definition requires at least two points")
	}

	// Check that at least one yk is greater than zero
	if (slices.forall { slice => slice.y0 == 0 && slice.y1 == 0 }) {
		throw new IllegalArgumentException("At least one yk must be non-zero")
	}

	// Computes min/max abscissas and y_max for the function
	val (a, b, ym) = {
		/** Extracts min and max abscissas, y_max for a given slice */
		def extractAbscissasAndYMax(s: Slice) = (s.x0, s.x1, s.y0 max s.y1)

		/** Given two tuple (x0, x1, y_max), computes a new tuple of overall min/max. */
		def minMax(a: (Double, Double, Double), b: (Double, Double, Double)) = {
			val (x0a, x1a, yma) = a
			val (x0b, x1b, ymb) = b
			(x0a min x0b, x1a max x1b, yma max ymb)
		}

		slices.map(extractAbscissasAndYMax).reduce(minMax)
	}

	/** Area under the function */
	lazy val area: Double = slices.foldLeft(0.0) { (a, slice) => a + slice.area }

	/**
	  * Constructs a discrete law associating each slice of the function to a probability defined
	  * as the ratio between the area of the slice and the area of the whole function.
	  */
	lazy val slicesLaw = slices.map { slice => (slice.area / area, slice) }

	/**
	  * Computes the expected value of a variable X ...
	  */
	lazy val expectedValue = slicesLaw.map { case (prob, slice) => prob * slice.expectedValue }.sum

	/** Returns the slice containing the given x value. */
	def sliceFor(x: Double): Slice = {
		@tailrec
		def search(lo: Int, hi: Int): Slice = {
			if (lo > hi) throw new IllegalArgumentException(s"Function is undefined for x = $x")

			val mid = (lo + hi) / 2
			val slice = slices(mid)

			if (slice.contains(x)) slice
			else if (x < slice.x0) search(lo, mid - 1)
			else search(mid + 1, hi)
		}

		search(0, slices.length - 1)
	}

	/** Evaluates the function for the given x value */
	def evaluate(x: Double) = sliceFor(x).evaluate(x)

	/**
	  * Returns a new FunctionDefinition (proportional to this one) with an area of 1,
	  * suitable to be used as a density function.
	  */
	lazy val proportionalDensity: FunctionDefinition = {
		/**
		  * Scales a slice of this function by the area of this function.
		  * In practice, we create a new slice with scaled y0 and y1 values.
		  */
		def scaleSlice(slice: Slice) = slice.copy(y0 = slice.y0 / area, y1 = slice.y1 / area)

		new FunctionDefinition(slices.map(scaleSlice)) {
			// Return the same object, since the function is already a density function. :)
			override lazy val proportionalDensity: FunctionDefinition = this
		}
	}
}
