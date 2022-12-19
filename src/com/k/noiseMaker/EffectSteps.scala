package com.k.noiseMaker;

class EffectSteps(name:String,val scaleFactor:Int) extends Effect(name){
	
	def this(e:EffectSteps)={//clone
		this("STP",e.scaleFactor)
	}
	
	override def duplicate():EffectSteps={// clone
		new EffectSteps(this)
	}
		
	def this() {
		this("STP",1)
	}
	
  def getEffectValue(track:Track, currentValue:Double, currentTime:Double):Double={
	  ((track.getSndValue(currentTime)*scaleFactor).round).toDouble/scaleFactor.toDouble
	}

	def setAttribute(attrName:String, attrValue:Any )={
		(attrName,attrValue) match {      
      case ("scaleFactor",v:Int) =>	new EffectSteps("STP",v)
      case _ => Logger.log("Uknown attribute:" + attrName);
		}
	}

	override def getInfo(tabs:String ):String = {
		super.getInfo(tabs)+", scaleFactor:"+scaleFactor
	}
		
	override def getSrcCode():String= {
		"val "+getPrefixeduid()+" = new EffectSteps("+scaleFactor+")\n"
	}
}
