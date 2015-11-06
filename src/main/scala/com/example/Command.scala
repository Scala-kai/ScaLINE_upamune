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
  def writeString(fileName: String, body: String): Unit = {
    val file = new PrintWriter(fileName)
    try {
      file.write(body)
    }finally{
      file.close
    }

  }

  override def run(): Unit = {
    val events = Main.getEvents()
    val nowSec = "%tQ" format new Date()
    var logs = ""
    for(event <- events) {
      logs += event.toString + "\n"
    }
    writeString(s"scaline_log_$nowSec", logs)
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
    val currentUser = Main.getCurrentUser()
    currentUser match {
      case Some(u) => println(u)
      case _ => println("please change user")
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
    val currentUser = Main.getCurrentUser()
    currentUser match {
      case Some(user) => {
        user.friendList.find(u => u.id == id) match {
          case Some(to) => {
            val msg = Message(user, to, text)
            val e = SendMessageEvent(msg, new Date())
            Main.addEvent(e)
          }
          case _ => println(s"$id not found in your friend list")
        }
      }
      case _ => println("please change user")
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
    if(msg.from.id == user.id) {
      var text = s"$time ${msg.text}"
      if(!msg.isNonRead()) {
        text = "* " + text
      }
      println(rjust(text, width))
    }else if(msg.to.id == user.id){
      var text = s"${msg.text} $time"
      if (msg.isNonRead()) {
        msg.MarkasRead()
      }
      println(ljust(text, width))
    }
  }

  override def run(): Unit = {
    // イベントからメッセージイベントだけをしぼりこんで, そのなかでさらに to か from にcurrentUserがふくまれているやつをリストにしてかえす
    val currentUser = Main.getCurrentUser()

    currentUser match {
      case Some(user) => {
        Main.getUserBy(id) match {
          case Some(x) => {
            val to = x
            val messageEvents = Main.getUserMessageEvent(user, to)
            if (messageEvents.nonEmpty){
              println(center(s"${to.name}(@${to.id})", width))
              insertSeparator(width)
              messageEvents.foreach(msgEvent => showMessages(user, msgEvent))
              insertSeparator(width)
            }else {
              println("Message not found.")
            }
          }
          case _ => println("user not found")
        }
      }
      case _ => println("please change user")
    }
  }
}

case class ShowFriends() extends Command{
  override def run(): Unit = {
    val currentUser = Main.getCurrentUser()
    currentUser match {
      case Some(user) => {
        val friendList = user.friendList
        if (friendList.isEmpty) println("friend not found")
        else friendList.foreach(u => println(u))
      }
      case _ => println("please change user")
    }
  }
}

case class FollowUser(id:String) extends Command{
  // TEL or Email or ID の判別をかしこく
  // parseInt(id) -> TEL
  // contain@(id) -> Email
  // (id) -> id
  override def run(): Unit = {
    val currentUser = Main.getCurrentUser()

    currentUser match {
      case Some(user) => {
        if(!Main.isExistUser(id)) {
          println(s"not found $id")
        }else if(id == user.id || id == user.tel || id == user.mail) {
          println("それはあなたです！")
        }else if(Main.isFollowed(id)){
          println("already followed")
        }else {
          val follower = user
          Main.getUserBy(id) match {
            case Some(x) => {
              val followee = x
              follower.follow(followee)
              followee.follow(follower)
              val e = FollowUserEvent(follower, followee, new Date())
              Main.addEvent(e)
              println(s"follow ${followee}")
            }
            case _ =>
          }
        }
      }
      case _ => println("please change user")
    }
  }
}

case class CommandNotFound() extends Command{
  override def run(): Unit = {
    println("invalid command")
    println(
      """COMMANDS:
        | create create user -> create `id` `name` `tel` `email`
        | change change user -> change `id`(or `tel` or `email`)
        | send   send message -> send `id` `text`
        | show   show messages -> show `id`
        | log    show logs -> log
        | export export logs -> export
        | list   show your friends -> list
        | who    show who am i -> who
        | follow follow user -> follow `id`
      """.stripMargin)
  }
}

