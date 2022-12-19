package com.k.noiseMaker

object NoiseMaker extends App {
  Logger.log("Let's go!");
  Logger.log("Preparing music");
  
  def track1():Track={
    val beat=.1;
    val nbrepeats=50;
    //tchtch
    val tch = new Track(new Oscillator("rnd",RND,.0,.0,1.0),new EffectEnveloppe(beat/10,beat/10,.8,beat/10,beat/10))
    val tchrpt = tch.addEffect(new EffectRepeat(beat*2, nbrepeats))
    //bass
    val bass = new Track(new Oscillator("B110",SIN,110.0,.0,1.0),new EffectEnveloppe(beat/10,beat/10,.8,beat/10,beat/10))
    val dblbass = new Track("HB",List((.0,bass),(beat,bass))).repeat(beat*4,nbrepeats)
    //doppler pop
    val pop = new Track("pop",List((.0,new Oscillator("sin440",SIN,440.0,.0,1.0))),new EffectEnveloppe(beat/10,beat/10,.8,beat/10,beat/10))
    val delay = new Track(new Oscillator("DO",SIN,1/(40*beat),.0,beat/2))//variable delay
    val poprpt = pop.repeat(delay,100)
    //full
    val t1 = new Track("Full",List((.0,tchrpt),(.0,dblbass),(.0,poprpt)))//combine all three starting at 0
    t1
  }
  
  //play
  val t1 = track1();
  Logger.log(t1)
  val synth1 = t1.play()
  
   def makeABC(f:Double):Track={
     val beat=.1;
     def makeNote(f:Double):Track={
       new Track(new Oscillator(f.toString(),SIN,f,.0,1.0),new EffectEnveloppe(beat/10,beat/10,.8,beat/10,beat/10)) 
     }
     import Array._
     new Track("ABC",range(0,3,1).map(i=>(i*beat,makeNote(f+f*.10*i))).toList)
   }
  makeABC(330).play()
  makeABC(630).play()
  
  //Thread.sleep(10*1000)//wait x secs
  synth1.stop()
  
  Logger.log("Bye!");
}
