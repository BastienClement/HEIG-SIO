package util

class Timer {
	private final val msFactor = 1000000.0
	private var start = now

	def now = System.nanoTime()

	/** microseconds since start */
	def time = (now - start) / msFactor

	def reset() = {
		val t = time
		start = now
		t / msFactor
	}
}
