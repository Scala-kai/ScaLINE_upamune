package com.example
import java.util.Date

/**
 * Created by upamune on 15/10/31.
 */

case class Message(from:User, to:User, text:String) {
  override def toString(): String = {
    s"Message{${from.id} -> ${to.id}: $text"
  }
}

