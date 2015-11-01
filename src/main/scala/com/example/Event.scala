package com.example
import java.util.Date

/**
 * Created by upamune on 15/10/31.
 */


trait Event{
  val date: Date
}

case class CreateUserEvent(user:User, date: Date) extends Event {
  override def toString():String = {
    s"CreatedUser,$user,$date"
  }
}

case class FollowUserEvent(to: User, from: User, date: Date) extends Event {
  override def toString():String = {
    s"Followed,${to.id} <=> ${from.id},$date"
  }
}

case class SendMessageEvent(msg:Message,date:Date) extends Event {
  override def toString():String = {
    s"SendMessage,$msg,$date"
  }
}

