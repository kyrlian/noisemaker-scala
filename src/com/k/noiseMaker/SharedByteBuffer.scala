package com.k.noiseMaker;

class SharedByteBuffer(size:Int) {
	val buf = Array.fill(size){0.toByte}
	var in = 0
	var out = 0
	var count = 0

  @throws(classOf[InterruptedException])
	def put(o:Byte)=synchronized{
		while (count == size) {
			//Logger.log("put:wait");
			wait();
		}
		//Logger.log("put:waitEnd");
		buf.update(in, o)
		count+=1
		in = (in + 1) % size;
		notifyAll();
	}

  @throws(classOf[InterruptedException])
	def get():Byte=synchronized{
		while (count == 0) {
			//Logger.log("get:wait");
			wait();
		}
		//Logger.log("get:waitEnd");
		count-=1
		out = (out + 1) % size;
		notifyAll();
		buf(out)
	}
  
  @throws(classOf[InterruptedException])
	def isFull():Boolean=synchronized{
		(count == size)
  }

  @throws(classOf[InterruptedException])
	def fillRate():Double=synchronized{
		(count.toDouble / size.toDouble)
  }  
  
}