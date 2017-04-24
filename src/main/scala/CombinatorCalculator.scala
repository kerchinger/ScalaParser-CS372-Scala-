package edu.luc.cs.laufer.cs473.expressions
import edu.luc.cs.laufer.cs473.expressions.behaviors._
import scala.collection.mutable.Map


import scala.language.postfixOps
import scala.util.Try


object CombinatorCalculator extends App {

  type Store = Map[String, LValue[Int]]
  var store: Store = Map.empty
    //Map("x" -> Cell(5), "y" -> Cell(6), "r" -> Cell(0))
  type Value = LValue[Int]

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.statement*, input)
    println("result " + result)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      val expr = result.get
      println("The parsed expression is: ")
      println(toFormattedString(expr))
      println("The unparsed expression is: ")
      println(PrettyPrinter.toFormattedString(expr))
     // println("It has size " + size(expr) + " and height " + height(expr)) looks like we don't need these!
      println("It evaluates to " + Try(evaluate(store)(expr)))
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    println("Memory: " + store)
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines foreach { line =>
      processExpr(line)
      println("Memory: " + store)
      print("Enter infix expression: ")
    }
  }
}
