package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._

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
  //def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ block ~ "else" ~ block ^^ { case _ ~ _ ~ e ~ _ ~ b ~ _ ~ d => Cond(e, b, d) }
  def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ opt(block | "else" ~ block) ^^ { //TODO DOES NOT WORK, definetly not correct
    case l ~ Some("if" ~ "(" ~ e ~ ")" ~ b) => Cond(e.asInstanceOf[Expr], b.asInstanceOf[Expr], b.asInstanceOf[Expr])
    case l ~ Some("if" ~ "(" ~ e ~ ")" ~ "else" ~ b) => Cond(e.asInstanceOf[Expr], b.asInstanceOf[Expr], b.asInstanceOf[Expr])
  }

  /** loop ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] = "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case e ~ _ ~ b => Loop(e, b) } //TODO DOES NOT WORK, i think this is correct
  /** block ::= "{" statement* "}" */
  def block: Parser[Expr] = "{" ~> (statement*) <~ "}" ^^ { case s => Block(s:_*) } // TODO THIS IS NEEDS TO BE CHANGED, I think this is also correct

}
