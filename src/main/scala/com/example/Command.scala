package com.example

import java.util.Date
import java.io.PrintWriter


/**
 *
 * Created by upamune on 15/10/26.
 */

sealed trait Command{
  def run(): Unit
}

case class ExportLog() extends Command {
  override def run(): Unit = {
    val events = Main.getEvents()
    val nowSec = "%tQ" format new Date()
    var logs = ""
    for(event <- events) {
      logs += event.toString + "\n"
    }
    val file = new PrintWriter(s"scaline_log_$nowSec")
    if(file != null) {
      file.write(logs)
      file.close()
    }
  }
}

case class CreateUser(id:String, name:String, tel:String, mail:String) extends Command{
  override def run(): Unit = {
    val u = User(id, name, tel, mail)
    if (Main.createUser(u)) {
      val e = CreateUserEvent(u, new Date())
      Main.addEvent(e)
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
      println(currentUser)
    }else{
      println("please change user")
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

case class ShowLog() extends Command {
  override def run(): Unit = {
    val events = Main.getEvents()

    events.foreach(e => println(e))
  }
}

case class SendMessage(id:String, text:String) extends Command{
  override def run(): Unit = {
    val currentUser = Main.currentUser
    val to = currentUser.friendList.find(u => u.id == id) match {
      case Some(u) => u
      case None => null
    }
    if(to == null){
      println(s"$id not found in your friend list")
    }else{
      val msg = Message(currentUser, to, text)
      val e = SendMessageEvent(msg, new Date())
      Main.addEvent(e)
    }
  }
}

case class ShowMessage(id:String) extends Command{
  val width = 30
  def ljust(s: String, n: Int): String = {
    if (s.length < n) {
      s + (1 to (n - s.length)).map{ _ => " " }.mkString
    } else {
      s
    }
  }

  def rjust(s: String, n: Int): String = {
    if (s.length < n) {
      (1 to (n - s.length)).map{ _ => " " }.mkString + s
    } else {
      s
    }
  }

  def center(s: String, n: Int): String = {
    if (s.length < n) {
      { ((1 to ((n - s.length) / 2)).map{ _ => " " }.mkString) +
        s +
        ((1 to (n - (((n - s.length) / 2) + s.length))).map{ _ => " "}.mkString)
      }
    } else {
      s
    }
  }

  def insertSeparator(width:Int): Unit = {
    val separetor = "=" * width
    println(separetor)
  }

  def showMessages(user: User, msgEvent:SendMessageEvent): Unit = {
    val msg = msgEvent.msg
    val time = "%tR" format msgEvent.date
    if(msg.from.id == user.id){
      val text = s"$time ${msg.text}"
      println(rjust(text, width))
    }else if(msg.to.id == user.id){
      val text = s"${msg.text} $time"
      println(ljust(text, width))
    }
  }
  override def run(): Unit = {
    // イベントからメッセージイベントだけをしぼりこんで, そのなかでさらに to か from にcurrentUserがふくまれているやつをリストにしてかえす
    val currentUser = Main.currentUser
    val to = Main.getUserBy(id)
    if(to == null) {
      println("message not found")
      return
    }

    val messageEvents = Main.getUserMessageEvent(currentUser, to)
    if (messageEvents.nonEmpty){
      println(center(s"${to.name}(@${to.id})", width))
      insertSeparator(width)
      messageEvents.foreach(msgEvent => showMessages(currentUser, msgEvent))
      insertSeparator(width)
    }else {
      println("Message not found.")
    }
  }
}

case class ShowFriends() extends Command{
  override def run(): Unit = {
    val currentUser = Main.currentUser
    if (currentUser == null) {
      println ("please change user")
    }else {
      val friendList = currentUser.friendList
      if (friendList.isEmpty) println("friend not found")
      else friendList.foreach(u => println(u))
    }
  }
}

case class FollowUser(id:String) extends Command{
  // TEL or Email or ID の判別をかしこく
  // parseInt(id) -> TEL
  // contain@(id) -> Email
  // (id) -> id
  override def run(): Unit = {
    val currentUser = Main.currentUser
    if(currentUser == null){
      println("please change user")
    }else if(!Main.isExistUser(id)) {
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
      val e = FollowUserEvent(follower, followee, new Date())
      Main.addEvent(e)
      println(s"follow ${followee}")
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

