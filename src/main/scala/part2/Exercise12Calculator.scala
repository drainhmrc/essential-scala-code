package part2

import java.io.Serializable


// ----------------------------------------------

// Step 1. Write a definition for Expr here!

sealed abstract class Expr extends Product with Serializable {
  def stringify : String

}


// Handle the following types of equation:
// - addition
// - subtraction
// - multiplication
// - division
// - square root

// Give it a `stringify` method
// that renders the expression as a string.

// ----------------------------------------------

// Step 2. Implement eval
// for each of the "calculator" objects below:

case class Addition(d: Expr, d2: Expr) extends Expr {
  override def stringify: String = s"${d.stringify} + ${d2.stringify}"
}
case class Subtraction(d: Expr, d2: Expr) extends Expr{
  override def stringify: String = s"${d.stringify} / ${d2.stringify}"
}
case class Multiplication(d: Expr, d2: Expr) extends Expr {
  override def stringify: String = s"${d.stringify} * ${d2.stringify}"
}
case class Division(d: Expr, d2: Expr) extends Expr{
  override def stringify: String = s"${d.stringify} / ${d2.stringify}"

}
case class SquareRoot(d: Expr) extends Expr{
  override def stringify: String = s"√${d.stringify}"
}

case class Constant(d: Double) extends Expr {
  override def stringify: String = s"$d"
}


object Calculator {
  def eval(calc: Expr): Double = {
    calc match {
      case SquareRoot(d) => Math.sqrt(eval(d))
      case Division(d, d2) => eval(d) / eval(d2)
      case Multiplication(d, d2) => eval(d) * eval(d2)
      case Addition(d, d2) => eval(d) + eval(d2)
      case Subtraction(d, d2) => eval(d) - eval(d2)
      case Constant(d) => d
    }
  }
}

object IntCalculator {


  def operate(d: Expr, d2: Expr)(f: (Int, Int)=> Int )= {
    for {
      a <- eval(d)
      b <- eval(d2)
    } yield f(a,b)
  }

  def eval(calc: Expr): Either[String ,Int] = {
    calc match {
      case SquareRoot(d) => eval(d).map(Math.sqrt(_).round.toInt)
      case Division(d, d2) =>
          for {
            a <- eval(d)
            b <- eval(d2)
            sanitisedB <- Either.cond(b != 0, b, "DivideByZero")
          } yield a / sanitisedB

      case Multiplication(d, d2) => operate(d, d2)(_ * _)

      case Addition(d, d2) =>  operate(d , d2)(_ + _)

      case Subtraction(d, d2) => operate(d , d2)(_ - _)

      case Constant(d) => Right(d.round.toInt)
      }
  }
}

// ----------------------------------------------

// Step 3. Write some convenience methods
// for constructing common calculations:

// ----------------------------------------------

object Expr {



  def pythag(a: Double, b: Double): Expr = {
     SquareRoot(Addition(Multiplication(Constant(a), Constant(a)),Multiplication(Constant(a), Constant(a))))
   }

   def factorial(n: Int): Expr = {
     if( n == 1 )
       Constant(1)
     else
       Multiplication(Constant(n), factorial(n - 1))
   }
}

object Exercise11Calculator {
   val calc1 = Addition(Constant(1.1), Multiplication(Constant(2.2), Constant(3.3)))
   val calc2 = Addition(Multiplication(Constant(1.1), Constant(2.2)), Constant(3.3))
   val calc3 = Division(Constant(1), Constant(0))

  def main(args: Array[String]): Unit = {
    println("stringify")
    println(calc1.stringify)
    println(calc2.stringify)

    println("Calculator.eval")
    println(Calculator.eval(calc1))
    println(Calculator.eval(calc2))

    println("IntCalculator.eval")
    println(IntCalculator.eval(calc1))
    println(IntCalculator.eval(calc2))

    println("pythag")
    println(Expr.pythag(3, 4))
    println(Calculator.eval(Expr.pythag(3, 4)))
    println(Expr.pythag(3, 4).stringify)
    println(IntCalculator.eval(Expr.pythag(3, 4)))

    println("factorial")
    println(Expr.factorial(6))
    println(Expr.factorial(6).stringify)
    println(Calculator.eval(Expr.factorial(6)))
    println(IntCalculator.eval(Expr.factorial(6)))

    println("divide by zero")
    println(calc3.stringify)
    println(IntCalculator.eval(calc3))
    println(IntCalculator.eval(calc2))
  }
}
