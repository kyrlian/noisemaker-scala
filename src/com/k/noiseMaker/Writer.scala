package com.k.noiseMaker;

import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;

object Writer{
  // convert an int to a byte array
  def intToByteArray(data:Int):Array[Byte]={
    val bytes = Array.fill(4){0.toByte}
    for (i <- 0 until 4) {
      bytes.update(i, (data >>> (i * 8)).toByte )// LE
    }
    bytes
  }

  def shortToByteArray(data:Short):Array[Byte]={
    //return new byte[] { (data & 0xff).toByte, ((data >>> 8) & 0xff).toByte }// LE
    val bytes = Array.fill(2){0.toByte}
    for (i <- 0 until 2) {
      bytes.update(i, (data >>> (i * 8)).toByte )// LE
    }
    bytes
  }  
}

class Writer( fpath:String,  sampleProvider:Sampler, sharedBuffer:SharedByteBuffer , secondsToSave:Int,sArea:SharedArea ) extends Thread {
	// Instance data
	var savedSamples = 0;
  Logger.log("Preparing Writer thread");
  val dummyByteBuffer = Array.fill(sampleProvider.bytesPerSample){0.toByte};
  val myPath = fpath;
  val samplesToSave = sampleProvider.samplesPerSecond * secondsToSave;
  val outFile = writeWavHeader(samplesToSave);
  Logger.log("Running Writer thread");
  start();
  sArea.put("Recording", true);//used by sampler to put in buffer
  
	def this(sArea:SharedArea) {
		this("out.wav", sArea.get("sampleProvider").asInstanceOf[Sampler], sArea.get("sharedWriteBuffer").asInstanceOf[SharedByteBuffer], 60,sArea);//TODO seconds to save will be optional
	}

	def writeWavHeader(nSamples:Int):DataOutputStream={
		val formatHeaderByteSize = 16;// 2+2+4+4+2+2;
		// int dataByteSize = nBuffers * sampleProvider.bytesPerBuffer;
		// TODO for real time we'll need to come back and set this at the end,
		// or write to a .part file, then on end generated header and concat
		val dataByteSize = nSamples * sampleProvider.nbChannels * 2;
		val fileByteSize = 4 + 4 + 4 + formatHeaderByteSize + 4 + 4 + dataByteSize;// 4 + 24 + (8 + M * Nc * Ns + (0 or 1))

			val outFile = new DataOutputStream(new FileOutputStream(myPath));
			// Header
			// Nc = nb channels = 2
			// Ns = nb blocks = nBuffers * samplesPerBuffer
			// F = blocks per second = samplesPerSecond
			// M*Nc = sample size = bytesPerSample
			outFile.writeBytes("RIFF"); // 00 - RIFF (that means all int should
										// be LittleEndian as RIFF is LE)
			outFile.write(Writer.intToByteArray(fileByteSize), 0, 4); // 04 - rest of file size
			outFile.writeBytes("ComplexOscillator"); // 08 - ComplexOscillator
			outFile.writeBytes("fmt "); // 12 - fmt
			outFile.write(Writer.intToByteArray(formatHeaderByteSize), 0, 4); // 16 size of this chunk (16)
			outFile.write(Writer.shortToByteArray(1), 0, 2); // 20 - 1 for PCM
			outFile.write(Writer.shortToByteArray(sampleProvider.nbChannels.toShort), 0, 2); // 22 - Mono 1, Stereo 2
			outFile.write(Writer.intToByteArray(sampleProvider.samplesPerSecond), 0, 4); // 24 samples per sec
			outFile.write(Writer.intToByteArray((sampleProvider.samplesPerSecond * sampleProvider.bytesPerSample)), 0, 4); // 28 - nb bytes per second - all channels
			outFile.write(Writer.shortToByteArray(sampleProvider.bytesPerSample.toShort), 0, 2); // 32 - nb bytes per sample (all channels)
			outFile.write(Writer.shortToByteArray((sampleProvider.bytesPerSample * 8 / sampleProvider.nbChannels).toShort), 0, 2); // 34 - nb bits per sample (PER channels) (bytes * 8)
			outFile.writeBytes("data"); // 36 - data
			outFile.write(Writer.intToByteArray(dataByteSize), 0, 4); // 40 - data bytes size
    outFile
	}

	override def run() {
		// Data
		var stop=false;
		while ((savedSamples < this.samplesToSave) && !stop) {
				for (i <- 0 until sampleProvider.bytesPerSample) {
					dummyByteBuffer.update(i,sharedBuffer.get())
				}
				outFile.write(dummyByteBuffer, 0, sampleProvider.bytesPerSample);
				savedSamples+=1;
			if ((sArea.get("StopRecord").toString()).equals("StopRecord")){
				sArea.put("Recording", false);//used by sampler
				//TODO finalise file header
				stop=true;
			}			
		}
		finish();
	}

	def finish() {
		outFile.flush();
		outFile.close();
		Logger.log("End of Writer Thread");
	}

	override def interrupt() {
		finish();
		super.interrupt();
	}

}