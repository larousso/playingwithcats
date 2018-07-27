package tests

import cats.implicits._

object CatsApplicative extends App {

  case class Test(arg1: Int, arg2: String, other: Boolean)

  private val curried: Option[Int => String => Boolean => Test] = (Test.apply _).curried.some

  val arg1 = 1.some
  val arg2 = "test".some
  val arg3 = true.some

  private val res: Option[Test] = curried <*> arg1 <*> arg2 <*> arg3
  println(res)
}