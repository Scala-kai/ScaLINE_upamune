package com.example

case class User(id:String, name:String, tel:String, mail:String) {
  var friendList = List[User]()

  def follow(u:User): Unit ={
    friendList = u :: friendList
  }

  def sendMsg(name:String): Unit ={
  }

  def showLog(): Unit ={
  }

  def exportLog(): Unit ={
  }
}
