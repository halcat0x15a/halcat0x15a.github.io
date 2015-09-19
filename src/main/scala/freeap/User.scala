package freeap

import scala.util.control.Exception.allCatch

case class User(username: String, fullname: String, id: Int)

object User {

  type FreeOpt[A] = Free[Opt, A]

  def username: FreeOpt[String] = Free.one(Opt("username", None, Some[String]))

  def fullname: FreeOpt[String] = Free.one(Opt("fullname", Some(""), Some[String]))

  def id: FreeOpt[Int] = Free.one(Opt("id", None, s => allCatch.opt(s.toInt)))

  def user: FreeOpt[User] = Applicative.map3(username, fullname, id)(User.apply)

}
