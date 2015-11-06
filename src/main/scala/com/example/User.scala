package com.example


case class User(id:String, name:String, tel:String, mail:String) {
  var friendList = List[User]()

  override def toString: String = {
    s"User{id: $id name: $name tel: $tel mail: $mail}"
  }

  def follow(u:User): Unit ={
    friendList = u :: friendList
  }

}
