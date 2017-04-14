package edu.luc.cs.laufer.cs473.expressions
import edu.luc.cs.laufer.cs473.expressions.behaviors._
import scala.language.postfixOps


object CombinatorCalculator extends App {

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
      //println("It has size " + size(expr) + " and height " + height(expr))
      //println("It evaluates to " + evaluate(expr))
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines foreach { line =>
      processExpr(line)
      print("Enter infix expression: ")
    }
  }
}
