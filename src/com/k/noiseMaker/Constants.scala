package com.k.noiseMaker;

object Constants {
    val bufferSize = 40960
		val MinSamplesPerPeriod = 50.0
		val MinFreq = 30//Hz
		val MaxFreq = 12000//Hz
		val FreqLogBase = 1.2//log
		val sliderScale = 100000		
		val fps = 60
		val msecWait:Long = 1000 / fps
		//Logarithm base change rule
		//	logb(x) = logc(x) / logc(b)
		//	logB(x) = log10(x) / log10(B)
		def logBase(logBase:Double, x:Double):Double={
			Math.log10(x) / Math.log10(logBase);
		}
				
}
