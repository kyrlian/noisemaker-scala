package com.k.noiseMaker;

class EffectReverb(name:String,val delay:Double) extends Effect(name){// implements Effect {
		
	def this()={
		this("RVRB",1.0);
	}
	def this(e:EffectReverb )={// clone
		this("RVRB",e.delay)
	}
	
	override def duplicate():EffectReverb ={// clone
		new EffectReverb(this)
	}
	
	def getEffectValue( track:Track,  currentValue:Double,  currentTime:Double):Double={	
		val pastTime = currentTime-delay;
		if(track.isActive(pastTime)){
			val pastValue = track.getValue(pastTime);
			currentValue + ( pastValue *  1/Math.pow(delay,2));
		}else currentValue
	}

	override def getEffectLength( currentLength:Double):Double={
		currentLength+delay	
	}
  
	def setAttribute( attrName:String, attrValue:Any ) {
		(attrName,attrValue) match {      
      case ("Delay",v:Double) => 	new EffectReverb("D"+v,v)
      case _ => Logger.log("Uknown attribute:" + attrName);
		}
	}
	
		override def getInfo(tabs:String ):String = {
				super.getInfo(tabs) +", delay:"+delay
	}
		
	override def getSrcCode():String= {
		"val "+getPrefixeduid()+" = new EffectReverb("+delay+")\n";
	}
}
