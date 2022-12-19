package com.k.noiseMaker;

import Array._

class Track(name:String,val hardLength:Option[Double], val soundList:List[(Double,Signal)],val effect:Option[Effect]) extends Signal(name) {
	// protected EffectEnveloppe env;
	val soundLen:Double = computeSoundLength()
	val effectLen:Double = computeEffectLength(soundLen)
  val estimatedMaxAmplitude:Double = estimateMaxAmplitude()
	
  val soundPeriod:Double = computeSoundPeriod()
  val effectPeriod:Double = computeEffectPeriod(soundPeriod)
  val startTime:Double=getStartTime()
  val endTime=startTime+math.max(effectLen,soundLen)
  
  def this(name:String,hardLength:Option[Double], soundList:List[(Double,Signal)]){
    this(name,hardLength,soundList,None)
  }
  
  def this(name:String,soundList:List[(Double,Signal)]){
    this(name,None,soundList,None)
  }

  def this(name:String,soundList:List[(Double,Signal)],e:Effect){
    this(name,None,soundList,Some(e))
  }
    
  def this(name:String) {
		this(name,None,List[(Double,Signal)](),None)
  }
  
	def this(t:Track)={// Clone constructor
    this(t.name+"clone",t.hardLength,t.soundList,t.effect)
	}

	override def duplicate():Track={// Clone method
		new Track(this);
	}

	def this(se:Signal)={
		this("T"+se.name,None,List((.0,se)),None)
	}
	def this(se:Signal, e:Effect, t:Double)={
		this("T"+se.name,None,List((t,se)),Some(e))
	}

	def this(se:Signal, t:Double)={
		this("T"+se.name,None,List((t,se)),None)
	}

	def this(se:Signal, e:Effect )= {
		this("T"+se.name,None,List((.0,se)),Some(e))
	}
	
	def this(freq:Double)={
    this(new Oscillator("SIN"+freq.toString(),SIN, freq, 0.0, 1.0))
  }
	
	def this()={
		this(.0)
  }
	   
	def this(fundFreq:Double, nHarmonics:Int)={
    this("H"+fundFreq.toInt,None,     
        range(1,nHarmonics+1,1).map{i =>
    			val iAmplitude = 1.0 / Math.pow(2, i);// Vary amplitude to get timbre
    			val iFreq = fundFreq * i;
    			(.0,new Oscillator(i.toString(),SIN, iFreq, 0.0, iAmplitude));
    		}.toList,
		  None)
	}



  def geteffect()={
    effect.get
  }
  
  override def getLength():Double={
    effectLen
  }
  
  def getStartTime():Double={
    if(soundList.size==0){
	    .0//Empty track
	  }else{
      soundList.map(p=>p._1).reduce(Math.min(_,_))
	  }
	}
  
	def computeSoundLength():Double={
	  if(hardLength.isDefined){
	    hardLength.get
	  }else if(soundList.size==0){
	    .0//Empty track
	  }else{
  		soundList.map(p=>(p._1+p._2.getLength())).reduce(Math.max(_,_))
	  }
	}

	def computeSoundPeriod():Double={
    if(soundList.size==0){
	    .0//Empty track
	  }else{
  		val subLen = soundList.map(p=>(p._1,p._2.getPeriod()))
 			subLen.map(p=>p._1+p._2).reduce(Math.max(_,_))
	  }
	}
		
	def computeEffectLength(sndLen:Double):Double={
	  if(hardLength.isDefined)
	    hardLength.get
	  else if(effect.isDefined)
 			effect.get.getEffectLength(sndLen)
 		else
 		  sndLen
	}

	def computeEffectPeriod(sndPeriod:Double):Double={
	  if(effect.isDefined)
 			effect.get.getEffectPeriod(sndPeriod)
 		else
 		  sndPeriod
	}
	
  def addSoundElement(o:Signal):Track={
    addSoundElement(.0,o)
  }
  def addSoundElement(t:Double,o:Signal):Track={
    new Track(name+o.name,hardLength,soundList:+(t,o),effect)
  }

	def addEffect(e:Effect):Track={
		//new Track(name,hardLength,soundList,Some(e))
	  new Track(name+e.name,None,List((.0,this)),Some(e))
	}

	def removeSoundElement(s:Signal):Signal={
    new Track(name,hardLength,soundList.filter(_._2!=s),effect)
	}
		
	def remove(uid:Int):Signal={
    NamedElement.getNamedElement(uid) match {
      case s:Signal => removeSoundElement(s)
      case e:Effect => if(effect==Some(e)){new Track(name,hardLength,soundList,None)}else{this}
    }
	}

	
	override def filterActiveTracks(fromTime:Double,toTime:Double):Signal={
    val tmp = soundList.filter(//filter oscillators with low freqs
      _ match {      
        case (s:Double,t:Track) => ((fromTime < t.endTime) || (t.startTime < toTime))
        case (s:Double,co:ComplexOscillator) => (s < toTime)
        case (s:Double,o:Oscillator) => (s < toTime)
        case _ => true
      }
    ).map(//create new tracks if needed
        to => to._2 match {      
         case t:Track => (to._1, t.filterActiveTracks(fromTime,toTime))
         case _ => to
        }    
    )
    new Track(name+"-FAT",hardLength,tmp,effect)	  
	}
	
	override def removeHighFreqs(fLimit:Double):Signal={
    val tmp = soundList.filter(//filter oscillators with low freqs
      _._2 match {      
        case co:ComplexOscillator => (co.getFreq() < fLimit)
        case o:Oscillator => (o.getFreq() < fLimit)
        case _ => true
      }
    ).map(//create new tracks if needed
        to => to._2 match {      
         case t:Track => (to._1, t.removeHighFreqs(fLimit))
         case _ => to
        }    
    )
    new Track(name+"-RHF",hardLength,tmp,effect)
  }

	def removeLowFreqs(fLimit:Double):Signal={
	  val tmp = soundList.filter(//filter oscillators with high freqs
	    _._2 match {      
        case co:ComplexOscillator => (co.getFreq() > fLimit)
        case o:Oscillator => (o.getFreq() > fLimit)
        case _ => true
      }
    ).map(//create new tracks if needed
        to => to._2 match {      
         case t:Track => (to._1, t.removeLowFreqs(fLimit))
         case _ => to
        }    
    )
		new Track(name+"-RLF",hardLength,tmp,effect)
	}

	def estimateMaxAmplitude():Double= {
		estimateMaxAmplitude(3)// 3 samples per second
	}

	def estimateMaxAmplitude(testsPerSecond:Double):Double={// better
		val tStep = 1.0 / testsPerSecond
		val tMax = Math.min(soundLen,60)// if infinite, look at 60 secs
		Range.Double(0.0,tMax,tStep).map(estimateAmplitude(_)).fold(.0)(Math.max(_,_))    
	}


	def estimateAmplitude(t:Double):Double = {
		if (isActive(t)) {
		  soundList.map(p=>p._2.estimateAmplitude(t - p._1)).reduce(_+_)
		}else{
		  .0
		}
	}

	def recurse(v:Double,f:Double=>Double,n:Int):Double={
    if(n==0)
      v
    else
      recurse(f(v),f,n-1)	  
	}
  
  def hardRepeat(delay:Signal,number:Int)={   
    val startList = (0 until number).map(i=>recurse(0,delay.getValue,i))
    val sList = startList.map(d=>(d,this)).toList
    new Track(this.name+"_r",sList)
  }

  def repeat(delay:Signal,number:Int)={
     this.addEffect(new EffectRepeat(delay, number))
  }
  def repeat(delay:Double,number:Int)={
     this.addEffect(new EffectRepeat(delay, number))
  }  
  
	def isActive( t:Double):Boolean= {
    val minStart = soundList.map( p => p._1).reduce(Math.min(_,_))
    (t >= minStart && t <= effectLen &&  
  		(if(effect.isEmpty){true}else{
  		  effect.get.isEffectActive(t)
  		})
    )
	}

	def getSndValue( t:Double):Double={// Get sound value at time t
		//if (isActive(t))  {
      soundList.map( p => p._2.getValue(t-p._1) ).fold(.0)(_+_)
		//}else
		//  .0
	}

	def getValue( t:Double):Double={// Get sound value at time t
	  //if(0 < t-t.toInt && t-t.toInt <.0001){//if t is near an entire second
	  //if(name=="HBR0.4" && 0 < t-t.toInt && t-t.toInt <.0001){//if t is near an entire second
	  //  Logger.log(this.getClassName()+" "+this.uid+":"+this.name+", t:"+t)
	  //}
		if (isActive(t)) {
      val sndValue = getSndValue(t)
			// apply effects
      if(effect.isDefined)
			  effect.get.getEffectValue(this, sndValue, t)
			else 
			  sndValue
		}else
		  .0
	}

	override def getFreqs(t:Double):List[Double]={
		if (isActive(t)) {
			val oTime = t % soundLen
			val freqs = soundList.map(p=>p._2.getFreqs(oTime-p._1)).flatten.toList
  		if (freqs.isEmpty) {// to avoid null later
        List(1.0)
      }else{
        freqs
      }
    }else{
      List()
    }
	}

  override def getFreqs():List[Double]={
    val freqs = soundList.map(p=>p._2.getFreqs()).flatten.toList
    if (freqs.isEmpty) {// to avoid null later
      List(1.0)
    }else{
      freqs
    }
  }
  
  override def getPeriod():Double={
    soundList.map(p=>p._2.getPeriod()).foldLeft(.0)(Math.max(_,_))
  }

	override def setAttribute(attrName:String , attrValue:String ):NamedElement={
		attrName match {
		  case "Name" => new Track(attrValue,hardLength,soundList,effect)
		}
	}


	override def getInfo(tabs: String ): String = {
		var info = super.getInfo(tabs) + ", soundLen:"+soundLen+ ", effectLen:"+effectLen+", estimatedMaxAmplitude:"+estimatedMaxAmplitude  
		if(effect.isDefined){
  		info += "\n" + tabs + "\\_Effect:"+effect.get.getInfo(tabs)
		}
		info += "\n" + tabs + "\\_Components:";
		for ((t,o) <- this.soundList) {
			info += "\n"+ tabs + "	|-" + o.getClassName+" at "+t+": "+o.getInfo(tabs+"	|	")
		}
		info
	}

	override def toString()={
    getInfo("");
  }

	override def getSrcCode():String={
		var sCode = "";
		if (!NamedElement.exportedIds.contains(uid)) {
			sCode += "val " + getPrefixeduid() + " = new Track();\n";
			// Add ComplexOscillators
			for ((t,o) <- this.soundList) {
				// keep a list of dumped elements to avoid redumping
				if (!NamedElement.exportedIds.contains(o.uid)) {
					sCode += o.getSrcCode();
					NamedElement.exportedIds+=o.uid;
				}
				sCode += getPrefixeduid() + ".addSoundElement("+t+","+ o.getPrefixeduid() + ")\n";
			}
			// Add effects
			if(effect.isDefined){
				if (!NamedElement.exportedIds.contains(effect.get.uid)) {
					sCode += effect.get.getSrcCode();
					NamedElement.exportedIds += effect.get.uid;
				}
				sCode += getPrefixeduid() + ".addEffect(" + effect.get.getPrefixeduid() + ");\n";
			}
			// sCode += "\n";
			NamedElement.exportedIds += uid
		}
		return sCode;
	}


	def getAmplitude(t:Double):Double={		
		estimateAmplitude(t)
	}

	def getAmplitude():Double={
		estimatedMaxAmplitude
	}
  
  override def getMaxAmplitude():Double={
    estimatedMaxAmplitude
  }
  
  def getSoundElement(i:Int):Signal={
    soundList(i)._2
  }
  
	def getFreq():Double={
		getFreqs().reduce(math.min(_,_))
	}
	
	def sample(start:Double,end:Double,step:Double):String={
	  Range.Double(start,end,step).map(getValue(_).toString()).fold("")(_+","+_)
	}
	
	def play():Synth={
	  val synth = new Synth(this);
	  synth.play();
	  synth
	}
	
}