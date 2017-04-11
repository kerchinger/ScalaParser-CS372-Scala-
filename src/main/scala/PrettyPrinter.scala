package edu.luc.cs.laufer.cs473.expressions
import edu.luc.cs.laufer.cs473.expressions.ast._

import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

object PrettyPrinter extends JavaTokenParsers {
  def toFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)

  // e = a sequence of zero or more Expr
  def toFormattedStrings(prefix: String)(e: Seq[_]): String = {
    val result = new StringBuilder(prefix)
    if (e.nonEmpty) {
      for (exp <- e) {
        result.append(toFormattedString(prefix)(exp.asInstanceOf[Expr]))
        result.append(EOL)
      }
    }
    result.toString()
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Variable(v)           => prefix + v.toString
    case Constant(c)           => prefix + c.toString
    case Assign(l, r)          => buildExprString(prefix, " = ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Cond(i, b, eb) => buildCondExprString(prefix, toFormattedString(prefix)(i),
      toFormattedString(prefix)(b), toFormattedString(prefix)(eb))
    case Loop(exp, b)          => buildLoopExprString(prefix, toFormattedString(prefix)(exp), toFormattedString(prefix)(b))
    // block always comes in as a block of a sequence of zero or more Expr
    case b: Block    => buildBlockExprString(prefix, toFormattedStrings(prefix)(b.statements))
    case UMinus(exp) => buildUnaryExprString(prefix, toFormattedString(prefix)(exp))
    case Plus(l, r)  => buildExprString(prefix, " + ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Minus(l, r) => buildExprString(prefix, " - ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Times(l, r) => buildExprString(prefix, " * ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Div(l, r)   => buildExprString(prefix, " / ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Mod(l, r)   => buildExprString(prefix, "%", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, opString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    opString match {
      case " = " =>
        result.append(leftString)
        result.append(opString)
        result.append(rightString)
        result.append(";")
      case _     =>
        result.append("(")
        result.append(leftString)
        result.append(opString)
        result.append(rightString)
        result.append(")")
    }
    result.toString()
  }

  def buildUnaryExprString(prefix: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append("(-")
    result.append(exprString)
    result.append(")")
    result.toString()
  }

  def buildBlockExprString(prefix: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    if (exprString.trim.length > 0) {
      result.append("{")
      result.append(EOL)
      // adds an indent to each line in the exprString
      result.append(exprString.lines.map(s => INDENT + s).mkString(EOL))
      result.append(EOL)
      result.append("}")
    }
    result.toString()
  }

  def buildLoopExprString(prefix: String, exprString: String, blockString: String) = {
    val result = new StringBuilder(prefix)
    result.append("while (")
    result.append(exprString)
    result.append(") ")
    result.append(blockString)
    result.toString()
  }

  def buildCondExprString(prefix: String, ifString: String, blockString: String, elseBlock: String) = {
    val result = new StringBuilder(prefix)
    result.append("if (")
    result.append(ifString.dropRight(1))
    result.append(") ")
    result.append(blockString)
    if (elseBlock.trim.length > 0) {
      result.append(" else ")
      result.append(elseBlock)
    }
    result.toString()
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "

}