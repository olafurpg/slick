package scala.slick.shadow.test

import scala.slick.shadow.Entity

object YYDefinitions {
  @Entity("COFFEES") case class Coffee(@Entity("COF_NAME") name: String, sales: Int, flavor: Option[String])
  @Entity("COFFEE") case class Coffee1(id: Int, name: String)
  @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String)
  @Entity("COFFEE") case class Coffn(@Entity("ID") idNumber: Int, @Entity("NAME") _2: String)
  @Entity("cat_j") case class Categories(@Entity("id") id: Int, @Entity("name") name: String)
  @Entity("posts_j") case class Posts(@Entity("id") id: Int, @Entity("title") title: String, @Entity("category") category: Int)
  @Entity("t3") case class T3(@Entity("a") a: Int, @Entity("b") b: Int)
  @Entity("t3o") case class T3O(@Entity("a") a: Int, @Entity("b") b: Option[Int])
  @Entity("SLICK_ACCOUNT") case class Account1(id: Int, balance: Int, transfers: String)
  @Entity("managers") case class Manager(@Entity("id") id: Int, @Entity("name") name: String, @Entity("department") department: String)
  @Entity("employees") case class Employee(@Entity("id") id: Int, @Entity("name2") name: String, @Entity("manager") manager: Int)
}