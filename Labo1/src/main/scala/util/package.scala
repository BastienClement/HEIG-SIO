package object util {
	/**
	  * Computes the mean of a sequence of Doubles
	  */
	implicit class DoubleSeqMean(val seq: Seq[Double]) extends AnyVal {
		@inline def mean = seq.sum / seq.length
	}
}
