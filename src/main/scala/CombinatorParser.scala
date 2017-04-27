package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._

import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

object CombinatorParser extends JavaTokenParsers {


  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] = (term ~ rep(("+" | "-") ~ term)) ^^ {
    case a ~ b => (a /: b) {
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] = (factor ~ rep(("*" | "/" | "%") ~ factor)) ^^ {
    case a ~ b => (a /: b) {
      case (x, "*" ~ y) => Times(x, y)
      case (x, "/" ~ y) => Div(x, y)
      case (x, "%" ~ y) => Mod(x, y)
    }
  }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" | ident */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
      | "+" ~> factor ^^ { case e => e }
      | "-" ~> factor ^^ { case e => UMinus(e) }
      | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
      | ident ^^ { case f => Variable(f) }
    )

  /** statement ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = (
    expr <~ ";" ^^ { case s => s }
      | assignment
      | conditional
      | loop
      | block
    )

  /** assignment ::= ident "=" expression ";" */
  def assignment: Parser[Expr] = ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(Variable(s), r) }

  /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  def conditional: Parser[Expr] = "if" ~> ("(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
    case e ~ b1 ~ None => Cond(e, b1, Block(): Block)
    case e ~ b1 ~ Some(b2) => Cond(e, b1, b2)
  }

  /** loop ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] = "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case e ~ _ ~ b => Loop(e, b) }

  /** block ::= "{" statement* "}" */
  def block: Parser[Expr] = "{" ~> (statement *) <~ "}" ^^ { case s => Block(s: _*) }

  /** struct ::= "{" "}" | "{" field { "," field }* "}" */
  def Struct: Parser[Expr] = (
    "{" ~ ident ~ "}" ^^ { case i => Variable(i.toString())}
    | "{" ~ field ~ rep1("{" ~ "," ~ field ) ~ "}" ^^ { case f ~ f2 => Struct(f, f2)}
  )

  /** field  ::= ident ":" expr */
  def field: Parser[Expr] = ident ~ ":" ~ expr ^^ { case i ~ _ ~ e => Field(i,e) }
}
