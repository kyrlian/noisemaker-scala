package com.k.noiseMaker;

class EffectRepeat(name:String, val repeatDelay:Signal, val repeatNumber:Int) extends Effect(name)   {

  def this(repeatDelay:Double, repeatNumber:Int){
    this("R"+repeatDelay,new Oscillator(FLAT,repeatDelay),repeatNumber)
  }   
    
  def this(repeatDelay:Signal, repeatNumber:Int){
    this("R"+repeatDelay.name,repeatDelay,repeatNumber)
  }
  
  def this(repeatNumber:Int){
    this(1.0,repeatNumber)
  }      
  
  def this(repeatDelay:Double){
    this(repeatDelay,10)
  }
    
	def this(e:EffectRepeat)={//clone
		this("REP",e.repeatDelay,e.repeatNumber)
	}
	
	override def duplicate():EffectRepeat={// clone
		new EffectRepeat(this)
	}
	
	def this() {
		this(1.0,10);
	}
		
  def getEffectValue(track:Track, currentValue:Double, currentTime:Double):Double={
  			if (currentTime <= track.soundLen) {
  			  currentValue
  			}else if (currentTime > repeatDelay(currentTime) ) {
  					track.getValue(currentTime % repeatDelay(currentTime));
  			}else{
  			  .0
  			}
	}

	override def getEffectLength(currentLength:Double):Double={
		repeatDelay.getMaxAmplitude() * repeatNumber
	}

	override def getEffectPeriod(sndLength:Double):Double={
		repeatDelay.getPeriod()
	}
	
	override def isEffectActive(currentTime:Double):Boolean= {
	  currentTime < (repeatDelay.getMaxAmplitude()  * repeatNumber)
	}

	def setAttribute(attrName:String, attrValue:Any )={
		(attrName,attrValue) match {      
      case ("RepeatDelay",v:Double) =>	new EffectRepeat(v, repeatNumber)
      case ("RepeatNumber",v:Int) =>	new EffectRepeat(repeatDelay, v)
      case _ => Logger.log("Uknown attribute:" + attrName);
		}
	}
	override def getInfo(tabs:String ):String = {
				super.getInfo(tabs) +", delay:["+repeatDelay.getInfo(tabs+"	") +"], repeatNumber:"+repeatNumber
	}
	
	override def getSrcCode():String= {
			"val "+getPrefixeduid()+" = new EffectRepeat(\""+name+"\","+repeatDelay+","+repeatNumber+")\n"
	}
}
