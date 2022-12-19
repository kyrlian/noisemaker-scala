package com.k.noiseMaker;

//common implementation to be reused among effects
abstract class Effect(name:String) extends NamedElement(name) {
	def getEffectLength(sndLength:Double):Double=	sndLength
	def isEffectActive(currentTime: Double): Boolean = true
  def getEffectPeriod(sndPeriod:Double):Double= getEffectLength(sndPeriod)
		
	def getEffectValue(track:Track, value:Double, myTime:Double):Double
  def setAttribute(attrName:String ,attrValue:Any)

}
