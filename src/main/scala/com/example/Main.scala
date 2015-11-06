package com.example
import scala.io.StdIn.readLine

object Main{
  private [this] var users = List[User]()
  private [this] var events = List[Event]()
  private [this] var currentUser:User = _

  def getCurrentUser(): Option[User] = {
    Option(currentUser)
  }

  def getEvents(): List[Event] = {
    events
  }

  def getUserMessageEvent(from:User, to:User): List[SendMessageEvent] = {
    var messageEvents = List[SendMessageEvent]()

    for(event <- events){
      event match {
        case msgEvent:SendMessageEvent => {
          val msg = msgEvent.msg
          if (msg.from.id == from.id && msg.to.id == to.id || msg.from.id == to.id && msg.to.id == from.id) {
            messageEvents = msgEvent :: messageEvents
          }
        }
        case _ =>
      }
    }

    messageEvents
  }

  def addEvent(e:Event): Unit = {
    events = e :: events
  }

  def createUser(u:User):Boolean = {
    if(isExistUser(u.id) || isExistUser(u.tel) || isExistUser(u.mail)){
      false
    }else{
      users = u :: users
      true
    }
  }

  def isExistUser(id:String): Boolean = {
    val u = getUserBy(id)
    u match {
      case Some(x) => true
      case _ => false
    }
  }

  def isFollowed(id:String): Boolean = {
    val user = getUserBy(id)
    val friendList = currentUser.friendList
    user match {
      case Some(u) => {
        friendList.exists(user => user.id == u.id)
      }
      case _ => false
    }
  }

  def getUserBy(id: String): Option[User] = {
    List(getUserById(id), getUserByTel(id), getUserByMail(id)).flatMap(x => x).distinct.headOption
  }

  def getUserById(id:String): Option[User] = {
    users.find(user => user.id == id)
  }

  def getUserByTel(tel:String): Option[User] = {
    users.find(user => user.tel == tel)
  }

  def getUserByMail(mail:String): Option[User] = {
    users.find(user => user.mail == mail)
  }

  def changeUser(id:String): Boolean ={
    val u = getUserBy(id)
    u match {
      case Some(x) => {
        currentUser = x
        true
      }
      case _ => false
    }
  }

  def main (args: Array[String]){

    print("cmd > ")

    Iterator.continually(scala.io.StdIn.readLine()).takeWhile(l => l != null).foreach {
      ln =>

        val createReg = """create\s(.*?)\s(.*?)\s(.*?)\s(.*?)""".r
        val changeReg = """change\s(.*?)""".r
        val sendReg = """send\s(.*?)\s(.*)""".r
        val followReg = """follow\s(.*?)""".r
        val showReg = """show\s(.*)""".r

        (ln match {
          case createReg(id,name,tel,mail) => CreateUser(id,name,tel,mail)
          case changeReg(id) => ChangeUser(id)
          case sendReg(id,text) => SendMessage(id,text)
          case showReg(id) => ShowMessage(id)
          case "list" => ShowFriends()
          case "export" => ExportLog()
          case "who" => WhoAmI()
          case "log" => ShowLog()
          case followReg(id) => FollowUser(id)
          case _ => CommandNotFound()
        }).run()

        print("cmd > ")
    }
  }
}
