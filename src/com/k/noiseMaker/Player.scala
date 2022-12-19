package com.k.noiseMaker;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;

class Player( sharedPlayBuffer:SharedByteBuffer, val samplesPerSecond:Int, secondsToPlay:Int,sArea:SharedArea ) extends Thread {
	Logger.log("Preparing Player thread");
	//reading setup, must be same as sampler
	val nbChannels = 2;// For now i have only 1 channel
	val bytesPerChannelSample = 2;// byte size for 1 sample for 1 channels (ie 1 short)
  val channelSamplesPerSecond = samplesPerSecond * nbChannels
  val bytesPerSample = bytesPerChannelSample * nbChannels// byte size for each sample for ALL channels (short=2 bytes)
  
  // Instance data	
	var playedSamples = 0
  val SIGNED = true
  val BIG_ENDIAN = false// WAV if little endian
  //AudioFormat(float sampleRate, int sampleSizeInBits, int channels, boolean signed, boolean bigEndian)
  //generated wav is : audioFormat: PCM_SIGNED 44100.0 Hz, 16 bit, stereo, 4 bytes/frame, little-endian
  //this.format = new AudioFormat(44100, 16, 2, SIGNED, BIG_ENDIAN);
  val dummyByteBuffer = Array.fill(bytesPerSample){0.toByte}//new ArraySeq[sampleProvider.bytesPerSample];
  val samplesToPlay = samplesPerSecond * secondsToPlay;
  val auSourceLine = initAudio();
  Logger.log("Running Player thread");
  start();  
  
	def this(sharedPlayBuffer:SharedByteBuffer,  samplesPerSecond:Int ,  sArea:SharedArea) {
		this(  sharedPlayBuffer, samplesPerSecond, -1, sArea) ;
	}
  
	def initAudio()={
		// Get line to write data to
      val format = new AudioFormat(samplesPerSecond.toFloat, bytesPerChannelSample * 8, nbChannels, SIGNED, BIG_ENDIAN);
      val sourceDataInfo = new DataLine.Info(classOf[SourceDataLine],format)
			val auSourceLine = AudioSystem.getLine(sourceDataInfo).asInstanceOf[SourceDataLine]
			auSourceLine.open(format);
			auSourceLine.start();
      auSourceLine
	}

	override def run() {
		while (this.samplesToPlay <0 || playedSamples < this.samplesToPlay) {
				//Logger.log("dummyByteBuffer:");
				for (i <- 0 until bytesPerSample) {
					val b:Byte = sharedPlayBuffer.get();
					dummyByteBuffer.update(i, b); 
					//Logger.loginline(((Byte)b).toString());
				}
				//Logger.log("");
				auSourceLine.write(dummyByteBuffer, 0, bytesPerSample);// to speakers
				playedSamples+=1
				//Logger.log("Played sample "+playedSamples);
				//Logger.log("dummyByteBuffer "+dummyByteBuffer.hashCode());
		}
		finish();
	}

	def finish() {
		auSourceLine.drain();
		auSourceLine.close();
		Logger.log("End of Player Thread");
	}

	override def interrupt() {
		finish();
		super.interrupt();
	}

}