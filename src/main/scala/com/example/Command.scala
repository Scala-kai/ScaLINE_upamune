package com.example

/**
 * Created by upamune on 15/10/26.
 */

sealed trait Command{
  def run(): Unit
}

case class CreateUser(id:String, name:String, tel:String, mail:String) extends Command{
  override def run(): Unit = {
    val u = User(id, name, tel, mail)
    if (Main.createUser(u)) {
      println("add user successfully")
    }else{
      println("add user failed")
    }
  }
}

case class WhoAmI() extends Command{
  override def run(): Unit = {
    val currentUser = Main.currentUser
    if( currentUser != null) {
      println(currentUser.name)
    }else{
      println("no user found")
    }
  }
}

case class ChangeUser(id:String) extends Command{
  override def run(): Unit = {
    if(Main.changeUser(id)){
      println("change user successfully")
    }else{
      println("change user failed")
    }
  }
}

case class SendMsg(id:String, text:String) extends Command{
  override def run(): Unit = {
  }
}

case class ShowFriends() extends Command{
  override def run(): Unit = {
    val currentUser = Main.currentUser
    val friendList = currentUser.friendList
    if(friendList.size == 0) println("friend not found")
    else friendList.foreach(u=> println(u))
  }
}

case class FollowUser(id:String) extends Command{
  // TEL or Email or ID の判別をかしこく
  // parseInt(id) -> TEL
  // contain@(id) -> Email
  // (id) -> id
  override def run(): Unit = {
    val currentUser = Main.currentUser
    if(!Main.isFoundUser(id)) {
      println(s"not found $id")
    }else if(id == currentUser.id || id == currentUser.tel || id == currentUser.mail) {
      println("それはあなたです！")
    }else if(Main.isFollowed(id)){
      println("already followed")
    }else {
      val follower = Main.currentUser
      val followee = Main.getUserBy(id)
      follower.follow(followee)
      followee.follow(follower)
      println(s"follow ${followee.name}")
    }
  }
}

case class CommandNotFound() extends Command{
  override def run(): Unit = {
    println("invalid command")
    println(
      """COMMANDS:
        | create create user -> create `id` `name` `tel` `email`
        | change change user -> change `id`
        | send   send message -> send `id` `text`
        | list   show your friends -> list
        | who    show who am i -> who
        | follow follow user -> follow `id`
      """.stripMargin)
  }
}
