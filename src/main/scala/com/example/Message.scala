package com.example

case class Message(from:User, to:User, text:String) {
  private [this] var read = false

  def isNonRead: Boolean ={
    !read
  }

  def MarkasRead(): Unit = {
    read = true
  }
  override def toString: String = {
    s"Message{${from.id} -> ${to.id}: $text"
  }
}

