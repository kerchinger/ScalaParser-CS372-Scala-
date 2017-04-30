package edu.luc.cs.laufer.cs473.expressions.ast

import scala.language.postfixOps
import scala.collection.immutable.Map

/** An initial algebra of arithmetic expressions. */
sealed trait Expr

case class Constant(value: Int) extends Expr

case class Variable(name: String) extends Expr

abstract class UnaryExpr(expr: Expr) extends Expr {
  require {
    expr != null
  }
}

case class UMinus(expr: Expr) extends UnaryExpr(expr)

abstract class BinaryExpr(left: Expr, right: Expr) extends Expr {
  require {
    (left != null) && (right != null)
  }
}

case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)

case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)

case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)

case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)

case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)

case class Block(statements: Expr*) extends Expr {
  //require(expressions != null)
  //require(!expressions.contains(null))
  //expressions: Expr*
}

case class Cond(guard: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr

case class Loop(guard: Expr, body: Expr) extends Expr

//case class Assign(left: Expr, right: Expr) extends Expr
case class Assign(left: Seq[Expr], right: Expr) extends Expr

//case class Field(left: String, right: Expr) extends Expr

case class Struct(fields: Map[Variable,Expr]) extends Expr

case class Select(expr: Expr, fieldSelector: Variable*) extends Expr
