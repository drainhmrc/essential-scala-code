package part2

import java.io.Serializable

import part2.Calculator.eval
import part2.IntCalculator.eval

import scala.collection.immutable.Range

// ----------------------------------------------

// Step 1. Write a definition for Expr here!

sealed abstract class Expr[A] extends Product with Serializable {
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

// Expr -
  // Double
  // Int
  // BigDecimal?

case class Addition[A](d: Expr[A], d2: Expr[A]) extends Expr[A] {
  override def stringify: String = s"${d.stringify} + ${d2.stringify}"
}

case class Subtraction[A](d: Expr[A], d2: Expr[A]) extends Expr[A]{
  override def stringify: String = s"${d.stringify} / ${d2.stringify}"
}

case class Multiplication[A](d: Expr[A], d2: Expr[A]) extends Expr[A] {
  override def stringify: String = s"${d.stringify} * ${d2.stringify}"
}

case class Division[A](d: Expr[A], d2: Expr[A]) extends Expr[A]{
  override def stringify: String = s"${d.stringify} / ${d2.stringify}"

}

case class SquareRoot[A](d: Expr[A]) extends Expr[A] {
  override def stringify: String = s"√${d.stringify}"
}

case class Constant[A](d: A) extends Expr[A] {
  override def stringify: String = s"$d"
}


object Calculator {
  def eval(calc: Expr[Double]): Double = {
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

  import cats.implicits._

  def operate(d: Expr[Int], d2: Expr[Int])(f: (Int, Int) => Int): Either[String, Int] = {
    (eval(d), eval(d2)).mapN(f)
  }

  def eval(calc: Expr[Int]): Either[String ,Int] = {
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

      case Constant(d) => Right(d)
      }
  }
}

object GenericCalculator {


  def operate[A: Numeric](d: Expr[A], d2: Expr[A])(f: (BigDecimal, BigDecimal) => BigDecimal): Either[String, BigDecimal] = {
    import cats.implicits._

    (eval[A](d), eval[A](d2)).mapN(f)
  }

  def eval[A](calc: Expr[A])(implicit numeric: Numeric[A]): Either[String, BigDecimal] =  calc match {
    case SquareRoot(d) => eval[A](d).map(n => BigDecimal(Math.sqrt(n.toDouble)))
    case Division(d, d2) =>
      for {
        unknownA <- eval[A](d)
        bigDecimalA = BigDecimal(unknownA.toString)
        unknownB <- eval[A](d2)
        bigDecimalB = BigDecimal(unknownB.toString)
        sanitisedB <- Either.cond(unknownB != numeric.zero, bigDecimalB, "DivideByZero")
      } yield {
        bigDecimalA / sanitisedB
      }
    case Multiplication(d, d2) => operate(d, d2)(_ * _)
    case Addition(d, d2) => operate(d, d2)(_ + _)
    case Subtraction(d, d2) => operate(d, d2)(_ - _)
    case Constant(d) => Right(BigDecimal(d.toString))
  }
}
// ----------------------------------------------

// Step 3. Write some convenience methods
// for constructing common calculations:

// ----------------------------------------------

object Expr {

  def pythag(a: Double, b: Double): Expr[Double] = {
     SquareRoot(Addition(Multiplication(Constant(a), Constant(a)),Multiplication(Constant(a), Constant(a))))
   }

   def factorial(n: Int): Expr[Int] = {
     if( n == 1 )
       Constant(1)
     else
       Multiplication(Constant(n), factorial(n - 1))
   }
}

object Exercise11Calculator {
   val calc1: Expr[Double] = Addition(Constant(1.1), Multiplication(Constant(2.2), Constant(3.3)))
   val calc2: Expr[Double] = Addition(Multiplication(Constant(1.1), Constant(2.2)), Constant(3.3))
   val calc3: Expr[Int] = Division(Constant(1), Constant(0))

  def main(args: Array[String]): Unit = {
    println("stringify")
    println(calc1.stringify)
    println(calc2.stringify)

    println("Calculator.eval")
    println(Calculator.eval(calc1))
    println(Calculator.eval(calc2))

    println("IntCalculator.eval")
    println(GenericCalculator.eval(calc1))
    println(GenericCalculator.eval(calc2))

    println("pythag")
    println(Expr.pythag(3, 4))
    println(Calculator.eval(Expr.pythag(3, 4)))
    println(Expr.pythag(3, 4).stringify)
    println(GenericCalculator.eval(Expr.pythag(3, 4)))

    println("factorial")
    println(Expr.factorial(6))
    println(Expr.factorial(6).stringify)
    println(GenericCalculator.eval(Expr.factorial(6)))
    println(IntCalculator.eval(Expr.factorial(6)))

    println("divide by zero")
    println(calc3.stringify)
    println(IntCalculator.eval(calc3))
    println(GenericCalculator.eval(calc2))
  }
}
