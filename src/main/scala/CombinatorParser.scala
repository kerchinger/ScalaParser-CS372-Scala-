package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._

import scala.util.parsing.combinator.JavaTokenParsers

object CombinatorParser extends JavaTokenParsers {




  /**conditional ::= "if" "(" expression ")" block [ "else" block ]*/
  //def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ block ~ "else" ~ block ^^ { case _ ~ _ ~ e ~ _ ~ b ~ _ ~ d => Cond(e, b, d) }
  /*def conditional: Parser[Expr] = "if" ~ "(" ~ expr ~ ")" ~ opt(block | "else" ~ block) ^^ { //TODO DOES NOT WORK, definetly not correct
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
*/

  /**assignment ::= ident "=" expression ";"*/
  def assignment: Parser[Expr] = ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(Variable(s), r) } // TODO THIS ALSO DOESN'T WORK, also think this is correct

  /**statement ::= expression ";" | assignment | conditional | loop | block*/
  def statement: Parser[Expr] = ( //TODO DOES NOT WORK, i have no idea
    expr <~ ";" ^^ {case s=> s}
      | assignment
    //| conditional
    //| loop
    //| block
    )

  /** expr ::= term { { "+" | "-" } term }* */
  /*def expr: Parser[Expr] =
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }*/

  /*def expr: Parser[Expr] =
    term ~! rep( ("+" | "-") ~ term ) ^^ {
      case t ~ l => l.foldLeft(t)(
       (res, h) => h match {
        case "-" ~ n => Minus(res, n)
        case "+" ~ n => Plus(res, n)
      }
    )
  }*/
  def expr: Parser[Expr] = (term ~ rep(("+" | "-") ~ term)) ^^ {
    case a ~ b => (a /: b) {
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }

  def term: Parser[Expr] = (factor ~ rep(("*" | "/" ) ~ factor)) ^^ {
    case a ~ b =>  (a /: b) {
      case (x, "*" ~ y) => Times(x, y)
      case (x, "/" ~ y) => Div(x, y)
      case (x, "%" ~ y) => Mod(x, y)

    }
  }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  /*def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case t ~ l => l.foldLeft(t)(
        (res, e) => e match {
          case "*" ~ n => Times(res, n)
          case "/" ~ n => Div(res,n)
          case "%" ~ n => Mod(res, n)
        }
      )
      /*case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)*/
    }*/

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" | ident */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case f => Variable(f) }
  )

}
