package com.k.noiseMaker;

//interface for ComplexOscillators or combination of (oscillator, complex, ComplexOscillator, tracks..)
abstract class Signal(name:String) extends NamedElement(name){
	def estimateMaxAmplitude():Double
	def getValue(t:Double):Double
	def getAmplitude():Double
	def estimateAmplitude(myTime:Double):Double
	def getFreqs(myTime:Double):Seq[Double] 
	def getFreqs():Seq[Double]
	def getLength():Double=Double.PositiveInfinity
	def getMaxAmplitude():Double={	  estimateMaxAmplitude()	}
	def removeHighFreqs(limit:Double):Signal={this}	
  def this()={this("")}
  def apply(t:Double):Double=getValue(t)
  def getPeriod():Double
  def filterActiveTracks(fromTime:Double,toTime:Double):Signal={ this }

}