package com.k.noiseMaker

class Sampler(var srcLeftRaw:Signal, var srcRightRaw:Signal, val samplesPerSecond:Int,sharedPlayBuffer:SharedByteBuffer, sharedWriteBuffer:SharedByteBuffer, secondsToSample:Int, sArea:SharedArea ) extends Thread {
  Logger.log("Preparing Sampler thread");
  val nbChannels = 2;// For now i have only 1 channel
	val bytesPerChannelSample = 2;// byte size for 1 sample for 1 channels (ie 1 short)
  var currentTime = .0
	var realMaxAmplitudeLeft = 1.0
	var realMaxAmplitudeRight = 1.0
	var sampledSamples = 0
  val channelSamplesPerSecond = samplesPerSecond * nbChannels
  val bytesPerSample = bytesPerChannelSample * nbChannels// byte size for each sample for ALL channels (short=2 bytes)
  val samplesToSample = samplesPerSecond * secondsToSample
  val timeStep = 1.0 / samplesPerSecond;
	Logger.log("timeStep:"+timeStep+" s");
  var srcLeft = cleanSource(srcLeftRaw.filterActiveTracks(.0,5.0));
	var srcRight = cleanSource(srcRightRaw.filterActiveTracks(.0,5.0));
  // Logger.log("Left:" + srcLeft.getInfo());
  // Logger.log("Right:" + srcRight.getInfo());
  //Logger.log(getInfo())
  Logger.log("Running Sampler thread")
  start()

	def this(srcLeft:Signal, srcRight:Signal, samplesPerSecond:Int,sharedPlayBuffer:SharedByteBuffer , sharedWriteBuffer:SharedByteBuffer  ,  sArea:SharedArea ) {
		this( srcLeft, srcRight,  samplesPerSecond, sharedPlayBuffer, sharedWriteBuffer, -1,  sArea);//-1:infinite
	}

  def addToLeftTrack(t:Double,newTrack:Signal){
    srcLeftRaw = new Track("Full",List((.0,srcLeftRaw),(t,newTrack)))
    srcLeft = cleanSource(srcLeftRaw)
  }
  def addToRightTrack(t:Double,newTrack:Signal){
    srcRightRaw = new Track("Full",List((.0,srcRightRaw),(t,newTrack)))
    srcRight = cleanSource(srcRightRaw);
  }    
  def addToTracks(t:Double,newTrack:Signal){
    addToLeftTrack(t,newTrack)
    addToRightTrack(t,newTrack)
  }
	def writeToBuffer(v:Double,scaleFactor:Double, buff:SharedByteBuffer ) {
		val ds = v * scaleFactor;
		//Logger.log("ds:"+ds);
		val ss:Short = Math.round(ds).toShort
		buff.put((ss >>> 8).toByte);// weird:in scala I have to put higher first  
		buff.put((ss.toByte))
	}

  def cleanSource(s:Signal):Signal={
    //Logger.log("cleanSource cut at:"+ samplesPerSecond.toDouble / Constants.MinSamplesPerPeriod)+" hz"
    s.removeHighFreqs(samplesPerSecond.toDouble / Constants.MinSamplesPerPeriod);// 50 = minimum number of points per period I want to have
  }

	override def run() {

		var scaleFactorLeft = Short.MaxValue / srcLeft.getMaxAmplitude() * .8;
		var scaleFactorRight = Short.MaxValue / srcRight.getMaxAmplitude() * .8;
		var scaleFactor = Math.min(scaleFactorLeft, scaleFactorRight);
		var nextUpdateTime = 0.0;
		//double debugOldVal=0.0;double debugOldTime=0.0;
		while (this.samplesToSample <0 || sampledSamples < this.samplesToSample) {
			currentTime = sArea.get("currentTime").asInstanceOf[Double]
			//update track info every few second
			if(currentTime > nextUpdateTime){
				nextUpdateTime += 5.0;//delay to update track infos
				srcLeft = cleanSource(srcLeftRaw.filterActiveTracks(currentTime , nextUpdateTime));
	      srcRight = cleanSource(srcRightRaw.filterActiveTracks(currentTime , nextUpdateTime));
				scaleFactorLeft = Short.MaxValue / srcLeft.getMaxAmplitude() * .8;
				scaleFactorRight = Short.MaxValue / srcRight.getMaxAmplitude() * .8;
				scaleFactor = Math.min(scaleFactorLeft, scaleFactorRight);
			}
			// current step
			//compute values
			val vLeft = srcLeft.getValue(currentTime);
			//Logger.log("Sample:left("+currentTime+")="+vLeft);
			//Logger.log("Sample:left="+vLeft);			
			realMaxAmplitudeLeft = Math.max(realMaxAmplitudeLeft, vLeft);
			val vRight = srcRight.getValue(currentTime);
			realMaxAmplitudeRight = Math.max(realMaxAmplitudeRight, vRight);
			//Logger.log("L:"+vLeft+", R:"+vRight);
			//player buffer
			writeToBuffer(vLeft, scaleFactor, sharedPlayBuffer);			
			writeToBuffer(vRight, scaleFactor, sharedPlayBuffer);
			//writer buffer
			val bRecording = sArea.get("Recording").asInstanceOf[Boolean]
			if(bRecording){
				writeToBuffer(vLeft, scaleFactor, sharedWriteBuffer);
				writeToBuffer(vRight, scaleFactor, sharedWriteBuffer);
			}
			sampledSamples+=1
			// Direction
			val PlayMode = sArea.get("PlayMode").asInstanceOf[String]
			var nextTime = currentTime;
			PlayMode match {
				case "Backward"=>	nextTime -= timeStep;
				case "Play" =>		nextTime += timeStep;
				case "Pause"=>  //sArea.standDown();//Pause
				case "Forward"=>	nextTime += (2.0 * timeStep);
			}
			if(currentTime == sArea.get("currentTime").asInstanceOf[Double]){//if nobody else changed it
				sArea.put("currentTime", nextTime);
			}
		}
		finish();
	}

	def finish() {
		Logger.log(getRunInfo());
		Logger.log("End of Sampler Thread");
	}

	override def interrupt(){
		finish();
		super.interrupt();
	}

	def reset(t:Double) {
		currentTime = t;
	}

	def reset() {
		reset(0.0);
	}

	def getInfo():String={
 		"Sampler:"+ 
		" nbChannels:" + nbChannels+
		", samplesPerSecond:" + samplesPerSecond +
		", bytesPerSample:" + bytesPerSample +
		", srcLeft.estimatedMaxAmplitude:" + srcLeft.getMaxAmplitude() +
		", srcRight.estimatedMaxAmplitude:" + srcRight.getMaxAmplitude() +
		"\nLeft track: "+srcLeft.getInfo()+
		"\nRight track: "+srcRight.getInfo()
	}

	def getRunInfo():String={
		"Sampler:" +
		", currentTime:" + currentTime +
		", srcLeft.estimatedMaxAmplitude:" + srcLeft.getMaxAmplitude() +
		", srcRight.estimatedMaxAmplitude:" + srcRight.getMaxAmplitude() +
		", realMaxAmplitudeLeft:" + realMaxAmplitudeLeft +
		", realMaxAmplitudeRight:" + realMaxAmplitudeRight
	}

}