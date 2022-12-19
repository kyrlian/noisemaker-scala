package com.k.noiseMaker

class Synth (val LRtracks:(Track,Track)){
    
  def this(t:Track){
    this((t,t))
  }

  val Ltrack = LRtracks._1
  val Rtrack = LRtracks._2

  val samplesPerSecond = 88200;//cd bitrate 44100
  //val samplesPerSecond = 44100;//cd bitrate 44100
  //val samplesPerSecond = 128000;//good kbps
  //val samplesPerSecond = 160000;//high kbps
  //val samplesPerSecond = 320000;//higher kbps
  //val secondsToSample = 10;//infinite if -1 or ommited
  //val secondsToSave = 10;//For writer
  //val secondsToPlay = secondsToSample;//For player - infinite if -1 or ommited
	// Prepare buffers
	Logger.log("Preparing buffers");
	val sArea = new SharedArea();
	//sArea.put("samplesPerSecond",samplesPerSecond);
	//sArea.put("secondsToSample",secondsToSample);
	sArea.put("currentTime", .0);
	val sharedPlayBuffer = new SharedByteBuffer(Constants.bufferSize);
	val sharedWriteBuffer = new SharedByteBuffer(Constants.bufferSize);
	//sArea.put("sharedWriteBuffer",sharedWriteBuffer);
	//Create Threads - run at launch
	val sampleProvider = new Sampler(Ltrack,Rtrack, samplesPerSecond, sharedPlayBuffer, sharedWriteBuffer,sArea);
	//sArea.put("sampleProvider", sampleProvider);
	//Wait a bit for the sampler to fill the buffer
	Thread sleep 100
	while(!sharedPlayBuffer.isFull){
	  Logger.log("Waiting for buffer to fill "+sharedPlayBuffer.fillRate)
	  Thread sleep 1000
	}
	//Run the player
	val player = new Player(sharedPlayBuffer,samplesPerSecond,sArea);// listen
	Logger.log("Everything is running, init is done.");
	
	def play(){
  	//Play
  	//sArea.put("Player", player);
    sArea.put("PlayMode", "Play");
    sArea.wakeUp();// UnPause
    // Done
	}
	
	def stop(){
	  player.finish()
	  sampleProvider.finish()
	}
	def getInfo():String={
	  "Synth:\n"+sArea.getInfo() + "\n" + sampleProvider.getInfo() +"\n"
	}
}
