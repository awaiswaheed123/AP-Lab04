
import scala.io.StdIn

object Lab04 extends App {
  println("Welcome to Mtarix multiplication")
  println("Enter Order of the Square Matrices")
  println("Order should be in power of 2.")
  print("n=");
  val n = StdIn.readInt()
  println("Enter 1st Matrix..")
  val m1 = Helper.populate(n)
  println("Enter 2nd Matrix..")
  val m2 = Helper.populate(n)
  println("1st Matrix:")
  Helper.display(m1)
  println("2nd Matrix:")
  Helper.display(m2)
  println("Multiplication Result:")
  Helper.display(Helper.starssen(m1,m2))
}