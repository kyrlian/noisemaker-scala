package com.k.noiseMaker;

import scala.collection.mutable._


sealed abstract class LFOType
case object FREQ extends LFOType
case object PHASE extends LFOType
case object AMPL extends LFOType


//an oscillator with lfo s
class ComplexOscillator(name:String,shape:ShapeType, freq:Double, phase:Double, amplitude:Double,val lfos:Map[LFOType,Signal]) extends Oscillator(name,shape,freq,phase,amplitude){
	// Constructors		
	def this(o:ComplexOscillator) {// clone
		this(o.name + "clone",o.shape, o.freq, o.phase, o.amplitude, o.lfos);
	}

  override def duplicate():ComplexOscillator={
    new ComplexOscillator(this)
  } 
  
	def this(o:Oscillator) {// upgrade from SignalSimpleOscillator
		this(o.name + "clone",o.shape, o.freq, o.phase, o.amplitude, new HashMap[LFOType,Signal]());
	}

	override def estimateMaxAmplitude():Double={
		if (lfos contains AMPL )
			getAmplitude() + lfos(AMPL).estimateMaxAmplitude()
		else
		  getAmplitude()
	}

	def setLFO(t:LFOType,lfo:Signal):Signal={
    new ComplexOscillator(name,shape,freq,phase,amplitude,lfos+(t->lfo))
	}

	def getLFO(t:LFOType):Signal={
		lfos(t)
	}

	
	override def  getSrcCode():String= {
		val sCode = "val " + getPrefixeduid() + " = new ComplexOscillator(" + getShape() + "," + getFreq() + "," + getPhase() + "," + getAmplitude() + ")\n";
		val lfoCode = lfos.foreach{pair => 
			if (!NamedElement.exportedIds.contains(pair._2.uid)) {
				pair._2.getSrcCode()
			} + getPrefixeduid() + ".setLFO(" + pair._1.toString() + "," + pair._2.getPrefixeduid() + ")\n"  
		}		  
		sCode+lfoCode
	}

	override def getFreqs(): Seq[Double]=  {		
		if (lfos contains FREQ ) {
      val p = freq + lfos(FREQ).estimateMaxAmplitude()
      val m = freq - lfos(FREQ).estimateMaxAmplitude()
      scala.collection.mutable.Seq[Double](freq,p,m)
		}else{
		  scala.collection.mutable.Seq[Double](freq)
    }
	}
	
	override def getFreq(t:Double):Double= {
		if (lfos contains FREQ ) {
			freq + lfos(FREQ).getValue(t)
		}else{
			freq
		}
	}
	
	override def getPeriod():Double={
	  if (lfos contains FREQ ) {
		  1/(	freq + lfos(FREQ).getMaxAmplitude() )
		}else{
			1/freq
		}
  }
		
		
	override def getAmplitude(t:Double):Double={
		if (lfos contains AMPL ) {
			amplitude + lfos(AMPL).getValue(t)
		}else{
			amplitude
		}
	}
	
	override def getPhase(t:Double):Double= {
		if (lfos contains PHASE ) {
			phase + 2.0 * Math.PI * lfos(PHASE).getValue(t)
		}else{
			phase
		}
	}
	
	override def getInfo(tabs:String):String= {
		super.getInfo(tabs)	//TODO should add lfo info
	}
	
	override def getInfo():String={
		getInfo("")
	}
  override def toString()={
    getInfo()
  }

}