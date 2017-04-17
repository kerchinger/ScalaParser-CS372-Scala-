package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._

import scala.language.postfixOps

import scala.collection.immutable.Map

/** Something that can be used on the right-hand side of an assignment. */
trait RValue[T] {
  def get: T
}

/** Something that can be used on the left-hand side of an assignment. */
trait LValue[T] extends RValue[T] {
  def set(value: T): LValue[T]
}

/** A cell for storing a value. */
case class Cell[T](var value: T) extends LValue[T] {
  override def get = value
  override def set(value: T) = { this.value = value; this }
}

/** A companion object defining a useful Cell instance. */
object Cell {
  val NULL = Cell(0)
}

/** An interpreter for expressions and statements. */
object Execute {

  type Store = Map[String, LValue[Int]]

  def apply(store: Store)(s: Expr): LValue[Int] = s match {
    case Constant(value)    => Cell(value)
    case Plus(left, right)  => Cell(apply(store)(left).get + apply(store)(right).get)
    case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
    case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
    case Div(left, right)   => Cell(apply(store)(left).get / apply(store)(right).get)
    case Variable(name)     => store(name)
    case Assign(left, right) => {
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    }
    case Block(statements @ _*) =>
      statements.foldLeft(Cell.NULL.asInstanceOf[LValue[Int]])((c, s) => apply(store)(s))
    case Loop(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
  }
}

object behaviors {

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r) => -evaluate(r)
    case Plus(l, r) => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r) => evaluate(l) / evaluate(r)
    case Mod(l, r) => evaluate(l) % evaluate(r)
    /*case Variable(e) => {}*/
    //case Block(l) => evaluate(l)
    //case Cond(l,r,x) =>
    //case Loop(l,r) =>
    //case Assign(l,r) => evaluate(r)
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r) => 1 + size(r)
    case Plus(l, r) => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r) => 1 + size(l) + size(r)
    case Mod(l, r) => 1 + size(l) + size(r)
    case Variable(s) => 1
    //case Block(l) => 1 + size(l)
    //case Cond(l,r,x) =>
    //case Loop(l,r) =>
    //case Assign(l,r) => 1 + size(r)
  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r) => 1 + height(r)
    case Plus(l, r) => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r) => 1 + math.max(height(l), height(r))
    case Mod(l, r) => 1 + math.max(height(l), height(r))
    case Variable(s) => 1
    //case Block(l) => 1 + height(l)
    // case Cond(l,r,x) =>
    //case Loop(l,r) =>
    //case Assign(l,r) => 1 + height(r)
  }

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
