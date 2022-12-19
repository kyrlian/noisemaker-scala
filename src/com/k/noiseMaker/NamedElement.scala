package com.k.noiseMaker;

import scala.collection.mutable._
import com.sun.xml.internal.bind.v2.runtime.RuntimeUtil.ToStringAdapter

object NamedElement{
  private var uidMax = 0;
	val nameList:Set[String] = Set()// List of taken names
	val hmElementsByUid:Map[Int,NamedElement] = new HashMap() //<Int, NamedElement>
	val hmElementsByName:Map[String,NamedElement]= new HashMap() //<String, NamedElement>
	val exportedIds:Set[Int]  = Set();
  
  def getNamedElement(id:Any):NamedElement={  
    id match {
      case sid:String => hmElementsByName.get(sid).get
      case iid:Int => hmElementsByUid.get(iid).get
      case _ => throw new ClassCastException
    }
  }

}

class NamedElement(val name:String){
	NamedElement.uidMax += 1;
  val uid= NamedElement.uidMax
	NamedElement.hmElementsByUid.put(uid, this);
  
	def this()={
	  this("")		
	}

	def this(e:NamedElement)={
		this(e.name + "clone")
	}
	
  def duplicate():NamedElement={
    new NamedElement(this)
  }

	def setName(n:String):NamedElement={
		if (NamedElement.nameList.contains(n)) {
			setName(n + "_" + uid) // name already taken
		} else {
			new NamedElement(n)
		}
	}
	
  def getuid():Int={
    uid
  }
	
	def getPrefix():String ={
    this match {
      case t:Track=>"t"
      case w:ComplexOscillator=>"w"
      case o:Oscillator=>"o"
      case e:Effect=>"e"
      case s:Signal=>"s"
      case _ => "uk"
    }
	}

	def setAttribute(attrName:String, attrValue:String ):NamedElement={
		attrName match {
	  	case "Name" =>	setName(attrValue);
	  	case _ =>//Nothing;
		}
		this
	}
	
	//Utils

	def getInfo(tabs:String ):String = {
		//no tabs, we continue same line
	  "uid:" + uid + ( if(name != ""){", Name:"+name}else{""} )
	}
		
	def getInfo():String = {
		getInfo("")
	}
	
  override def toString()={
    getInfo()
  }


	def getSrcCode():String = {
		"val "+uid+" = new BaseNamedElement()\n"
	}
	
	def getSrcCodeReset() {
		NamedElement.exportedIds.clear()
	}

	def  getPrefixeduid():String = {
		getPrefix()+uid+name;
	}
	
	def getClassName():String={
	  getClass.getName.toString().split('.').last
	}

}