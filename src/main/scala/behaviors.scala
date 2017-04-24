package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._

import scala.collection.mutable.Map
import scala.util.Try


/**
  * Something that can be used on the right-hand side of an assignment.
  */
trait RValue[T] {
  def get: T
}

/**
  * Something that can be used on the left-hand side of an assignment.
  */
trait LValue[T] extends RValue[T] {
  def set(value: T): LValue[T]
}

/**
  * A cell for storing a value.
  */
case class Cell[T](var value: T) extends LValue[T] {
  override def get = value
  override def set(value: T) = { this.value = value; this }
}

/**
  * A companion object defining a useful Cell instance.
  */
object Cell {
  val NULL = Cell(0)
}


object Execute {

  type Store = Map[String, LValue[Int]]

  def apply(store: Store)(s: Expr): LValue[Int] = s match {
    case Constant(value)    => Cell(value)
    case Plus(left, right)  => Cell(apply(store)(left).get + apply(store)(right).get)
    case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
    case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
    case Div(left, right)   => Cell(apply(store)(left).get / apply(store)(right).get)
    case UMinus(value) => Cell(-apply(store)(value).get)
    case Mod(left, right) => Cell(apply(store)(left).get % apply(store)(right).get)
    //variable case only if key is already found in map
    case Variable(name)     => store(name)
    //TODO find a way to add a value to the store with assign

    case Assign(left, right) =>
      val rValue = apply(store)(right)
      if (store contains left.asInstanceOf[Variable].toString) {
        val lValue = apply(store)(left)
        lValue.set(rValue.get)
        println("Got in but didn't change")
      }
      else {
        store.put(left.asInstanceOf[Variable].toString, rValue)
        println("Did not get in")
      }
      Cell.NULL

    case Block(statements @ _*) =>
      statements.foldLeft(Cell.NULL.asInstanceOf[LValue[Int]])((c, s) => apply(store)(s))
    case Loop(guard, body) =>
      var gValue = apply(store)(guard)
      while (gValue.get != 0) {
        apply(store)(body)
        gValue = apply(store)(guard)
      }
      Cell.NULL
    /*case Cond(Assign(left,right), block, elseBlock) =>
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      val result = Cell.NULL
      if(rvalue.get == lvalue.get) {
        result.set(apply(store)(block).get)
      }
      else {
        if(elseBlock.expr.nonEmpty) {
          result.set(apply(store)(elseBlock).get)
        }
      }
      result*/
  }
}

object behaviors {
  type Value = LValue[Int]
  type Store = Map[String, LValue[Int]]
  val result: Value = Cell.NULL

  def evaluate(store: Store)(e: Seq[_]): Value = {
    result.set(0)
    if(e.nonEmpty) {
      for (exp <- e) {
        result.set(Try(Execute(store)(exp.asInstanceOf[Expr])).get.get)
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
