package com.k.noiseMaker;

class EffectAmplify(name:String,val factor:Double) extends Effect(name){// implements Effect {
		
	def this()={
		this("AMPL",1.0);
	}

	def this(f:Double)={
		this("AMPL"+f,f)
	}  
		
	def this(e:EffectAmplify )={
		this("AMPL"+e.factor,e.factor)
	}  
 
	override def duplicate():EffectAmplify = {// clone
		new EffectAmplify(this)
	}
	
	override def getEffectValue(track:Track , sndValue:Double, time:Double):Double={
		sndValue*factor
	}
	
	def setAttribute(attrName:String ,attrValue:Any)={
		(attrName,attrValue) match {
		case ("Factor",v:Double) => 	new EffectAmplify(v)
    case _ =>Logger.log("Uknown attribute:" + attrName);
		}
	}
	
	override def getSrcCode():String={
		"val "+getPrefixeduid()+" = new EffectAmplify("+factor+")\n";
	}
}
