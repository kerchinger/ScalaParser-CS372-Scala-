package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._
import edu.luc.cs.laufer.cs473.expressions.evaluate.{Cell, Num}

import scala.collection.mutable.Map
import scala.util.Try
object evaluate {

  import ast._

  case class Cell(var value: Value) {
    def get: Value = value
    def set(value: Value): Unit = this.value = value
  }

  object Cell {
    def apply(i: Int): Cell = Cell(Num(i))
    val NULL = Cell(0)
  }

  type Instance = Map[String, Cell]

  type Store = Instance

  type Result = Try[Value]

  sealed trait Value

  case class Num(value: Int) extends Value

  def binOp(store: Store, left: Expr, right: Expr, op: (Int, Int) => Int): Cell ={
  val l: Int = apply(store)(left).value.asInstanceOf[Num].value
  val r: Int = apply(store)(right).value.asInstanceOf[Num].value
  Cell(Num(op(l, r)))
}

  def apply(store: Store)(s: Expr): Cell = s match {
    case Constant(value)    =>  { Cell(value) }
    case Plus(left, right)  =>   binOp(store, left, right, _ + _)
    case Minus(left, right) =>  { binOp(store, left, right, _ - _) }
    case Times(left, right) =>  { binOp(store,left, right, _ * _) }
    case Div(left, right)   =>  { binOp(store,left, right, _ / _) }
    case Mod(left, right)   =>  { binOp(store,left, right, _ % _) }
    case UMinus(expr)       =>  { Cell(Num(- expr.asInstanceOf[Constant].value)) }
    case Variable(name)     =>  { store(name) }
    case Assign(left, right) =>
      val rValue = apply(store)(right)
      if (store contains left.asInstanceOf[Variable].toString) {
        val lValue = apply(store)(left)
        lValue.set(rValue.get)
      }
      else {store.put(left.asInstanceOf[Variable].name, rValue)}
      Cell.NULL
    case Block(statements @ _*) =>
      statements.foldLeft(Cell.NULL)((c, s) => apply(store)(s))
    case Loop(guard, body) =>
      var gValue = apply(store)(guard)
      while (gValue.value != Num(0)) {
        apply(store)(body)
        gValue = apply(store)(guard)
      }
      Cell.NULL
      /**TODO SO I KIND OF CHANGED THE STRUCTURE OF THIS, but we can change it back  */
    case Cond(Assign(left, right), block, elseBlock) =>
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      val result = Cell.NULL
      if(rvalue.get == lvalue.get) {
        result.set(apply(store)(block).get)
      }
      else {
        if(elseBlock != result) {
          result.set(apply(store)(elseBlock).get)
        }
      }
      result
  }
}
object behaviors {

  type Value = Cell
  type Store = Map[String, Cell]
  val result: Value = Cell.NULL

  def execute(store: Store)(e: Seq[_]): Cell = {
    result.set(Num(0))
    if(e.nonEmpty) {
      for (exp <- e) {
        result.set(Try(evaluate(store)(exp.asInstanceOf[Expr])).get.get)
      }
    }
    result
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "


  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c) => prefix + c.toString
    case UMinus(r) => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r) => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r) => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r) => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r) => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r) => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))

    case Variable(s) => prefix + s.toString

    case b: Block => buildBlockExprString(prefix, toFormattedStrings(prefix )(b.statements ))
    case Cond(l, r, x) => buildCondExprString(prefix, toFormattedString(prefix )(l),
      toFormattedString(prefix)(r), toFormattedString(prefix )(x))
    case Loop(l, r) => buildLoopExprString(prefix, toFormattedString(prefix )(l), toFormattedString(prefix )(r))
    case Assign(l, r) => buildExprString(prefix, "Assign", toFormattedString(prefix )(l), toFormattedString(prefix )(r))
  }



  def toFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def toPrettyFormattedString(e: Expr): String = toFormattedString("")(e)

  def toPrettyFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)


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

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  def buildBlockExprString(prefix: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    if (exprString.trim.length > 0) {
      result.append("{")
      result.append(EOL)
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
}
