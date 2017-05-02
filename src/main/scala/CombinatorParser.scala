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

  /** factor ::= ident { "." ident }* | number | "+" factor | "-" factor | "(" expr ")" | struct */
  def factor: Parser[Expr] = (
    (ident | struct) ~ rep("." ~> ident) ^^ {
      case (f:String) ~ f2 => Select(Variable(f), f2.map(Variable))
      case (f: Expr) ~ f2 => Select(f, f2.map(Variable))}
      | wholeNumber ^^ { case s => Constant(s.toInt) }
      | "+" ~> factor ^^ { case e => e }
      | "-" ~> factor ^^ { case e => UMinus(e) }
      | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
      | struct ^^ { case f => f}
    )

  /** statement ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = (
    expr <~ ";" ^^ { case s => s }
      | assignment
      | conditional
      | loop
      | block
    )

  /** assignment  ::= ident { "." ident }* "=" expression ";" */
  def assignment: Parser[Expr] =  (ident | struct) ~ rep("." ~> ident) ~ ("=" ~> expr) <~ ";" ^^ {
    case (x: String) ~ t ~ r => Assign(Select(Variable(x), t.map(Variable)), r)
    case (x: Expr) ~ t ~ r => Assign(Select(x, t.map(Variable)), r)
  }

  /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  def conditional: Parser[Expr] = "if" ~> ("(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
    case e ~ b1 ~ None => Cond(e, b1, Block(): Block)
    case e ~ b1 ~ Some(b2) => Cond(e, b1, b2)
  }

  /** loop ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] = "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case e ~ _ ~ b => Loop(e, b) }

  /** block ::= "{" statement* "}" */
  def block: Parser[Expr] = "{" ~> (statement *) <~ "}" ^^ { case s => Block(s: _*) }

  def field: Parser[(String, Expr)] = ident ~ ":" ~ expr ^^ { case i ~ _ ~ e => i -> e }

  /** struct ::= "{" "}" | "{" field { "," field }* "}" */
  def struct: Parser[Expr] = (
        "{" ~ "}" ^^ { case _ ~ _ =>  Struct(Map() ) }
          | "{" ~> rep1sep(field, ",")  <~ "}" ^^ { case f => Struct(Map(f:_*))})
}


//factor ::= ident { "." ident }* | number | "+" factor | "-" factor | "(" expr ")" | struct
//ident ~ rep("." ~> ident) ^^ { case f ~ f2 => Select(Variable(f), f2.map(Variable)) }
//rep1sep(ident, ".") ^^ { case f => f.map(Variable).asInstanceOf[Expr]}

/** factor       ::= simplefactor { "." ident }*    */
//      | ident ~ rep(ident <~ ".") ^^ { case f ~ f2 => Select(Variable(f), f2.asInstanceOf[Variable]) }
//def factor2: Parser[Expr] =

//assignment  ::= ident { "." ident }* "=" expression ";"
//def assignment: Parser[Expr] = repsep(ident, ".") ~ "=" ~ expr ~ ";" ^^ { case t ~ _ ~ r ~ _ => Assign(t.asInstanceOf[Seq[Expr]], r) }
//def assignment: Parser[Expr] = ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(s.asInstanceOf[Seq[Expr]], r) }

//Struct(Map(null.asInstanceOf[Variable] -> null.asInstanceOf[Expr]))   :Map[Variable,Expr]  => f map { case (key, value) => Struct(key:_*) }
//f.foldRight(f.head)((_, b) => b)
//::f

/** field  ::= ident ":" expr */
//(i, e).asInstanceOf[Expr]

