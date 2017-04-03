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
    | ident ^^ {case s => Variable(s) }
  )

  /**statement ::= expression ";" | assignment | conditional | loop | block*/
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ {case s ~ _ => s}
    | assignment
    | conditional
    | loop
    | block
  )

  /**assignment ::= ident "=" expression ";"*/
  def assignment: Parser[Expr] = ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(s, r) }

  /**conditional ::= "if" "(" expression ")" block [ "else" block ]*/
  def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ block ~ "else" ~ block ^^ { case _ ~ _ ~ e ~ _ ~ b ~ _ ~ d => Cond(e, b, d) }
  /*I think this might be wrong... but, idk, i tried to model it off of the example below (the example is a parser but just written differently):
 def Cond = rule {
    "if" ~ WhiteSpace ~ ws('(') ~ Assignment ~ ws(')') ~ (
      (Blo ~ "else" ~ WhiteSpace ~ Blo ~> (Conditional(_: Equals, _: Block, _: Block)))
        | (Blo ~> (Conditional(_: Equals, _: Block, Block(): Block)))
    )
  }
   */

  /**loop ::= "while" "(" expression ")" block*/
  def loop: Parser[Expr] = "while" ~ "(" ~ expr ~ ")" ~ block ^^ { case _ ~ _ ~ e ~ _ ~ b => Loop(e, b) }

 /** block ::= "{" statement* "}"*/
  def block: Parser[Expr] = "{" ~ statement ~ "}" ^^ { case _ ~ s ~ _ => Block(s) }
}
