package com.k.noiseMaker;

import java.util.HashMap;

class SharedArea {
	val hm = new HashMap[String, Any]//TODO Any->Any ?? Some ??
	put("PlayMode", "Pause")
	put("Recording", false)

	def put(key:String,value:Any)=synchronized{
		hm.put(key, value);
		//Logger.log("hm:"+hm);
	}
	
	def get(key:String):Any=synchronized{
		if(hm.containsKey(key)){ 
			hm.get(key);
		}else "UNASSIGNED"
	}
	
		def getInfo():String={
		"Shared Area:"+ hm.toString()
		//hm.keySet().toArray().map(k=> k+":"+hm.get(k).toString()+"\n").foldLeft("")(_+_)
		}
		
  @throws(classOf[InterruptedException])
	def standDown(time:Long)=synchronized{
		wait(time);
	}
  
  @throws(classOf[InterruptedException])
	def standDown()=synchronized{			
		wait()
	}
	
  @throws(classOf[InterruptedException])
	def wakeUp()=synchronized{
		notifyAll();
	}
	
	def shutdown(){//unneeded for now		
		put("Shutdown","Yes");//set shutdown flag
		wakeUp();//raise all threads
	}
}
