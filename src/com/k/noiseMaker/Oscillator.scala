package com.k.noiseMaker;

import scala.util._
import scala.collection.mutable._

sealed abstract class ShapeType
case object SIN extends ShapeType
case object SQR extends ShapeType
case object SAW extends ShapeType
case object RND extends ShapeType
case object FLAT extends ShapeType


class Oscillator(name:String,val shape: ShapeType,val freq:Double,val phase:Double,val amplitude:Double) extends Signal(name) {
	//protected double amplitude;//in base
	val myRnd:Random = new Random();

	val periode:Double =	shape match {
				case FLAT => .0
				case SIN => 1 / freq
				case SQR =>	1 / freq
				case SAW=> 1 / freq
				case RND => 1.0
			}
	
	// Constructors
	def this(o:Oscillator)={// clone
		this(o.name + "clone",o.shape, o.freq, o.amplitude, o.phase)
	}

  override def duplicate():Oscillator={
    new Oscillator(this)
  } 
   
	def this(name:String,shape:ShapeType , value:Double)={
	  //if FLAT create flat with amplitude=value
	  //else create with freq=value
		this(name,shape,if(shape!=FLAT){value}else{.0}, .0, if(shape!=FLAT){.1}else{value})
	}
		
	def this(shape:ShapeType , value:Double)={
	  this(shape.toString()+value.toString(),shape , value)
	}
	
   def this(freq:Double)={
    this("SIN"+freq.toString(),SIN, freq, 0.0, 1.0)
  }
    
	def this(name:String,freq:Double)={
		this(name,SIN, freq, 0.0, 1.0)
	}

	def this (name:String) {
		this(name,SIN, 440, 0.0, 1.0);
	}
	
	// Modifiers
	
	
	def setShape(newshape:ShapeType):Oscillator={
		new Oscillator(name,newshape, freq, phase, amplitude)
	}
	
	def setShape(shape:String):Oscillator={
		shape match {
			case "FLAT" => setShape(FLAT)
			case "SIN" => setShape(SIN)
			case "SQR" => setShape(SQR)
			case "SAW" =>	setShape(SAW)
			case "RND" => setShape(RND)
		}
	}
	
	override def setAttribute( attrName:String, attrValue:String ):Signal={
		super.setAttribute(attrName, attrValue);
		attrName match {
			case "Amplitude" => new Oscillator(name,shape, freq, phase, attrValue.toDouble);
			case "Shape" => setShape(attrValue);
			case "Freq"  => new Oscillator(name, shape, attrValue.toDouble, phase, amplitude);
			case "Phase" => new Oscillator(name, shape, freq, attrValue.toDouble, amplitude);
      case "Name"  => new Oscillator(attrValue,shape, freq, phase, amplitude);
		}
	}
	
	// Getters

	def getAmplitude():Double={
		amplitude
	}


	def getAmplitude(t:Double):Double={
		amplitude
	}
	

	def estimateAmplitude(t:Double):Double={
		getAmplitude(t);
	}	

	def estimateMaxAmplitude():Double={
		getAmplitude();
	}


	def getValue(t:Double):Double={
		if (t >= 0) {
		  val f = getFreq(t)//use accessors, because can be overriden if complexOscillator
		  val p = getPhase(t)// phase should be -1 - 1			
		  val x = t * f + p
			val value = shape match {
				case FLAT => 1.0
				case SIN => Math.sin(2.0 * Math.PI * x)				//sin(2*pi*t * sin(2*pi*t*.003*f))
				case SQR =>	if (x % 2.0 < 1.0) {1.0} else {	-1.0}
				case SAW=> 2.0 * (x - Math.floor(x + 0.5))
				case RND => (myRnd.nextDouble() * 2.0) - 1.0
			}
		  //Logger.log("SignalSimpleOscillator:getValue:("+t+")="+(getAmplitude(t) * value))
			getAmplitude(t) * value			
		}else .0
	}

	def getFreqs(t:Double):Seq[Double]= {
		getFreqs()
	}
	
	def getFreqs():Seq[Double] = {
		Seq[Double](freq)// Arrays.asList(f));
	}

	def getFreq():Double={
		freq
	}

	override def getPeriod():Double={
    1/freq
  }
	  
	def getFreq(t:Double):Double={
		freq
	}
	
	def getShape():String={
		shape.toString();
	}

	def getPhase():Double={
		phase
	}
	
	def getPhase(t:Double):Double={
		phase
	}
	
	// Utils  
  override def toString()={
    getInfo()
  }
  
	override def getInfo(tabs:String ):String = {
		super.getInfo("") + ", amplitude:" + amplitude + ", shape:" + shape + ", freq:" + freq + ", phase:" + phase
	}


	override def getSrcCode():String={
		"val " + getPrefixeduid() + " = new "+ this.getClass.getCanonicalName +"(" + getShape() + "," + getFreq() + "," + getPhase() + "," + getAmplitude() + ")\n"
	}
	
}