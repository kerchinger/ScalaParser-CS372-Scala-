package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ {case s => Variable(s) } // TODO need to add Variable to ast.scala
  )
  /**statement ::= expression ";" | assignment | conditional | loop | block*/
  //def statement: Parser[Expr] = (
    //expr ~ ";" ^^ { case s => s} // TODO NOT CORRECT but a good start, change case statement
  //)
  /**assignment ::= ident "=" expression ";"*/
  //def assignment: Parser[Expr] = (
    //ident ~ "=" ~ expr ~ ";" ^^ {case s => Variable(s)} // TODO NOT CORRECT but a good start, change case statement
  //)
  /**conditional ::= "if" "(" expression ")" block [ "else" block ]*/
  //def conditional: Parser[Expr] = (
    //"if" ~ whiteSpace ~ "(" ~ expr ~ ")" ~ block ~> (Loop(_: Expr, _: Block))
  //)
  /**loop ::= "while" "(" expression ")" block*/
  //def loop: Parser[Expr] = (

  //)
 /** block ::= "{" statement* "}"*/
  //def block: Parser[Expr] = (

  //)
}
