package edu.luc.cs.laufer.cs473.expressions.ast

/** An initial algebra of arithmetic expressions. */
sealed trait Expr
case class Constant(value: Int) extends Expr
case class Variable(name: String) extends Expr
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr { require { (left != null) && (right != null) } }
case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Block(expressions: Expr*) extends Expr {
  require(expressions != null)
  require(!expressions.contains(null))
}
case class Cond(guard: Expr, thenBranch: Expr, esleBranch: Expr) extends Expr // TODO I question whether this should be guard, then, else; maybe it should be then, else
case class Loop(guard: Expr, body: Expr) extends BinaryExpr(guard, body)
case class Assign(left: Expr, right: Expr) extends BinaryExpr(left, right) // left could also be left:String