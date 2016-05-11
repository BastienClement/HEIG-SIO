package sampler

trait FunctionSampler {
	def apply(a: Double, b: Double, iter: Int): Interval
}
