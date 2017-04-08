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

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" | ident */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case f => Variable(f) } //TODO get "scala.MatchError: Variable(y)" when running this
  )

  /**statement ::= expression ";" | assignment | conditional | loop | block*/
  def statement: Parser[Expr] = ( //TODO DOES NOT WORK, i have no idea
    expr <~ ";" ^^ {case s => s}
    | assignment
    | conditional
    | loop
    | block
  )

  /**assignment ::= ident "=" expression ";"*/
  def assignment: Parser[Expr] = ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(Variable(s), r) } // TODO THIS ALSO DOESN'T WORK, also think this is correct

  /**conditional ::= "if" "(" expression ")" block [ "else" block ]*/
  //def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ block ~ "else" ~ block ^^ { case _ ~ _ ~ e ~ _ ~ b ~ _ ~ d => Cond(e, b, d) }
  def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ opt(block | "else" ~ block) ^^ { //TODO DOES NOT WORK, definetly not correct
    case l ~ Some("if" ~ "(" ~ e ~ ")" ~ b) => Cond(e.asInstanceOf[Expr], b.asInstanceOf[Expr], b.asInstanceOf[Expr])
    case l ~ Some("if" ~ "(" ~ e ~ ")" ~ "else" ~ b) => Cond(e.asInstanceOf[Expr], b.asInstanceOf[Expr], b.asInstanceOf[Expr])
  }
  /*I think this might be wrong... but, idk, i tried to model it off of the example below (the example is a parser but just written differently):
 def Cond = rule {
    "if" ~ WhiteSpace ~ ws('(') ~ Assignment ~ ws(')') ~ (
      (Blo ~ "else" ~ WhiteSpace ~ Blo ~> (Conditional(_: Equals, _: Block, _: Block)))
        | (Blo ~> (Conditional(_: Equals, _: Block, Block(): Block)))
    )
  }
   */

  /**loop ::= "while" "(" expression ")" block*/
  def loop: Parser[Expr] = "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case e ~ _ ~ b => Loop(e, b) } //TODO DOES NOT WORK, i think this is correct
 /** block ::= "{" statement* "}"*/
  def block: Parser[Expr] = "{" ~> (statement*) <~ "}" ^^ { case s  => Block(s:_*) } // TODO THIS IS NEEDS TO BE CHANGED, I think this is also correct

}
