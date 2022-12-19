package com.k.noiseMaker;


class EffectEnveloppe (name:String,val attackDuration:Double,val decayDuration:Double,val sustainValue:Double,val sustainDuration:Double,val releaseDuration:Double) extends Effect(name) {
	val decayDurationSum = attackDuration + decayDuration;
	val sustainDurationSum = decayDurationSum + sustainDuration;
	val releaseDurationSum = sustainDurationSum + releaseDuration;

	def this(e:EffectEnveloppe)={// Clone
		this("ENV",e.attackDuration,e.decayDuration, e.sustainValue, e.sustainDuration, e.releaseDuration);
	}

	def this() {
		this("ENV",.1, .2, .8, .6, .1);
	}
	
	def this(attackDuration:Double,decayDuration:Double,sustainValue:Double,sustainDuration:Double,releaseDuration:Double)={
		this("ENV",attackDuration,decayDuration, sustainValue, sustainDuration, releaseDuration);
	}	
	
	override def duplicate()={// clone
		new EffectEnveloppe(this)
	}

	override def getEffectLength(currentLength:Double):Double={
			Math.min(currentLength, releaseDurationSum)
	}

	 def getLength():Double={
		releaseDurationSum
	}
	
	override def isEffectActive(currentTime:Double):Boolean= {
		currentTime < releaseDurationSum
	}

	def getValue(currentTime:Double):Double= {
		val t = currentTime;
		if (t < attackDuration) {// 0-1
			(t / attackDuration);
		} else if (t < decayDurationSum) {// 1-sustain
			(t - decayDurationSum) * (1 - sustainValue) / (-1 * decayDuration) + sustainValue;
		} else if (t < sustainDurationSum) {// sustain
			sustainValue;
		} else if (t < releaseDurationSum) {// sustain-0
			sustainValue * (t - releaseDurationSum) / (-1 * releaseDuration);
		} else {// after enveloppe
			0.0;
		}
	}
	
	def getEffectValue( track:Track, currentValue:Double, currentTime:Double):Double={
		currentValue * getValue(currentTime);
	}

	 // Modifiers
	def setAttackDuration( newattackDuration:Double)={
		new EffectEnveloppe ("ENV",newattackDuration, decayDuration,sustainValue,sustainDuration, releaseDuration)
	}
	def setDecayDuration( newdecayDuration:Double) {
		new EffectEnveloppe ("ENV",attackDuration, newdecayDuration,sustainValue,sustainDuration, releaseDuration)
	}
	def setSustainValue(newsustainValue:Double) {
		new EffectEnveloppe ("ENV",attackDuration, decayDuration,newsustainValue,sustainDuration, releaseDuration)
	}
	def setSustainDuration(newsustainDuration:Double) {
		new EffectEnveloppe ("ENV",attackDuration, decayDuration,sustainValue,newsustainDuration, releaseDuration)
	}
	def setReleaseDuration(newreleaseDuration:Double) {
		new EffectEnveloppe ("ENV",attackDuration, decayDuration,sustainValue,sustainDuration, newreleaseDuration)
	}

	def setAttribute( attrName:String,attrValue: Any )={
		(attrName,attrValue) match { 			
			case ("Name",v:String) => setName(v)
			case ("AttackDuration",v:Double)=>		setAttackDuration(v)
			case ("DecayDuration",v:Double)=>		setDecayDuration(v)
			case ("SustainValue",v:Double)=>     setSustainValue(v)
			case ("SustainDuration",v:Double)=>  setSustainDuration(v)
			case ("ReleaseDuration",v:Double) =>	setReleaseDuration(v)
      case _ => Logger.log("Uknown attribute:" + attrName);
		}
		// notifyAll();
	}

	override def getInfo(tabs:String ):String = {
		super.getInfo(tabs) +
		  ", attackDuration:" + attackDuration +
		  ", decayDuration:" + decayDuration +
		  ", sustainValue:" + sustainValue +
		  ", sustainDuration:" + sustainDuration +
		  ", releaseDuration:" + releaseDuration
	}

	override def getSrcCode():String={
		 "val "+getPrefixeduid()+" = new EffectEnveloppe("+attackDuration+", "+ decayDuration+", "+ sustainValue+", "+ sustainDuration+", "+releaseDuration+")\n"		
	}
	
	def estimateMaxAmplitude():Double={
		1.0
	}
}