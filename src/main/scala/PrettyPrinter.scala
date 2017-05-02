package edu.luc.cs.laufer.cs473.expressions
import edu.luc.cs.laufer.cs473.expressions.ast._

import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

object PrettyPrinter extends JavaTokenParsers {
  def toFormattedString(e: Seq[Expr]): String = toFormattedStrings(e)

  def toFormattedStrings(e: Seq[_]): String = {
    val result = new StringBuilder
    if (e.nonEmpty) {
      for (exp <- e) {
        result.append(toFormattedString(exp.asInstanceOf[Expr]))
        result.append(EOL)
      }
    }
    result.toString()
  }
  def toFormattedStrings2(e: collection.Map[Variable,Expr]): String = {
    val result = new StringBuilder
    if (e.nonEmpty) {
      for ((k,v) <- e) {
        result.append(toFormattedString(k.asInstanceOf[Expr]))
        result.append(" : ")
        result.append(toFormattedString(v.asInstanceOf[Expr]))
        result.append(EOL)
      }
    }
    result.toString()
  }


  def toFormattedString(e: Expr): String = e match {
    case Constant(c) => c.toString
    case Variable(v) => v.toString
    case UMinus(exp) => buildUnaryExprString( toFormattedString(exp))
    case Plus(l, r)  => buildExprString( " + ", toFormattedString(l), toFormattedString(r))
    case Minus(l, r) => buildExprString( " - ", toFormattedString(l), toFormattedString(r))
    case Times(l, r) => buildExprString( " * ", toFormattedString(l), toFormattedString(r))
    case Div(l, r)   => buildExprString( " / ", toFormattedString(l), toFormattedString(r))
    case Mod(l, r)   => buildExprString( "%", toFormattedString(l), toFormattedString(r))
    case Assign(l, r)=> buildExprString( " = ", toFormattedString(l), toFormattedString(r))
    case Cond(i, b, eb) => buildCondExprString( toFormattedString(i), toFormattedString(b), toFormattedString(eb))
    case Loop(l, r) => buildLoopExprString(toFormattedString(l), toFormattedString(r))
    case Block(children @_*) => buildBlockExprString(toFormattedStrings(children))
    case Struct(m) => buildStructExprString(toFormattedStrings2(m))
    case Select(l, r) => buildSelectString( toFormattedString(l), toFormattedStrings(r))
  }

  def buildExprString( opString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder
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

  def buildUnaryExprString( exprString: String) = {
    val result = new StringBuilder
    result.append("(-")
    result.append(exprString)
    result.append(")")
    result.toString()
  }
  def buildSelectString( s1: String, s2: String) = {
    val result = new StringBuilder
    result.append(s1)
    if(s2.trim.length >= 0){
      result.append(s2.lines.map(s => "." + s).mkString(EOL))
    }
    result.toString()
  }
  def buildBlockExprString( exprString: String) = {
    val result = new StringBuilder
    if (exprString.trim.length >= 0) {
      result.append("{")
      result.append(EOL)
      result.append(exprString.lines.map(s => INDENT+ s).mkString(EOL))
      result.append(EOL)
      result.append("}")
    }
    result.toString()
  }

  //change blockString:String to blockString: Block
  def buildLoopExprString( exprString: String, blockString: String) = {
    val result = new StringBuilder
    result.append("while (")
    result.append(exprString)
    result.append(") ")
    result.append(blockString)
    result.toString()
  }

  //Change String in the block to Block
  def buildCondExprString (ifString: String, blockString: String, elseBlock: String) = {
    val result = new StringBuilder
    result.append("if (")
    result.append(ifString)
    result.append(") ")
    result.append(blockString)
    if (elseBlock.trim.length > 0) {
      result.append(" else ")
      result.append(elseBlock)
    }
    result.toString()
  }
  def buildStructExprString( m: String) = {
    val result = new StringBuilder
    if (m.trim.length > 0) {
      result.append("{")
      result.append(EOL)
      result.append(m) //.lines.map(s => INDENT + m).mkString(EOL))
      result.append(EOL)
      result.append("}")
    }
    result.toString()
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "
}