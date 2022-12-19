package com.k.noiseMaker;

import Array._

object SoundBlocks {

	 def chromaticScale()={
		val e = new EffectEnveloppe(.1, .1, .5, .3, .1);
		val A = 220;
		val semitone = A / 12;
		val soundList = range(0,12,1).map(i => (
      i * e.getLength() / 2.0 + .5,
			new Track(new Oscillator(A + i * semitone)).addEffect(e)
    )).toList
		new Track("chromaticScale",soundList)
	}

	def chromaticScaleHarmonics()={
		val e = new EffectEnveloppe(.1, .2, .5, .4, .1)
		//LFO lfo = new LFO(SIN,.5,.01);
		val A = 220
		val semitone = A / 12.0
    val soundList = range(0,12,1).map(i => (i*e.getLength()/1.5+.5,new Track(A+i*semitone, 4).addEffect(e)) ).toList
    new Track("chromaticScaleHarmonics",soundList)
	}
	 
	 def chromaticScaleReverse()={
		val e = new EffectEnveloppe(.1, .1, .5, .3, .1);
		//LFO lfo = new LFO(SIN,.5,.01);
		val A = 220;
		val semitone = A / 12.0;
		val soundList = range(0,12,1).map(i => (
			(12-i) * e.getLength() / 2.0 + .5, 
      new Track(new Oscillator(A + i * semitone)).addEffect(e)// 300 Hz, with enveloppe, starting at 2.0 seconds
		)).toList
		new Track("chromaticScaleReverse",soundList)
	}
	 
	 def harmonics()={
		val e = new EffectEnveloppe(.1, .1, .5, .3, .1);
		new Track(220, 4).addEffect(e);
	}

	 def whiteNoise()={
		 new Track(new Oscillator("rnd",RND, 1.0, 1.0, 1.0))
	 }
	 
	 def simpleTrack()={
		val e = new EffectEnveloppe(.1, .1, .5, .1, .1);
		val w440 = new ComplexOscillator(new Oscillator("w440",440))//ComplexOscillator at 440hz	
		new Track("s440",None,List((.01,w440)), Some(e));//ComplexOscillator at 440hz, with enveloppe
	 }
	 
	 
}
