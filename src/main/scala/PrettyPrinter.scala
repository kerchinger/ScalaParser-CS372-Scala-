package edu.luc.cs.laufer.cs473.expressions
import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

object PrettyPrinter extends JavaTokenParsers {
  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Any] = (term ~ rep(("+" | "-") ~ term)) ^^ {
    case a ~ b => (a /: b) {
      case (x, "+" ~ y) => (x + "+" + y)
      case (x, "-" ~ y) => (x + "-" + y)
    }
  }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Any] = (factor ~ rep(("*" | "/" | "%") ~ factor)) ^^ {
    case a ~ b => (a /: b) {
      case (x, "*" ~ y) => (x + "*" + y)
      case (x, "/" ~ y) => ( x +" /" + y)
      case (x, "%" ~ y) => (x + "%" + y)
    }
  }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" | ident */
  def factor: Parser[Any] = (
    wholeNumber ^^ { case s => s.toInt }
      | "+" ~> factor ^^ { case e => e }
      | "-" ~> factor ^^ { case e => e }
      | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
      | ident ^^ { case f => f }
    )

  /** statement ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Any] = (
    expr <~ ";" ^^ { case s => (s + ";") }
      | assignment
      | conditional
      | loop
      | block
    )

  /** assignment ::= ident "=" expression ";" */
  def assignment: Parser[Any] = ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => (s + "=" + r + ";")}

  /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  def conditional: Parser[Any] = "if" ~> ("(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
    case e ~ b1 ~ None => ("if" +"(" + e + ")" + EOL +b1)
    case e ~ b1 ~ Some(b2) => ("if" +"(" + e + ")" + EOL +b1 + EOL+ "else" + EOL+b2)
  }

  /** loop ::= "while" "(" expression ")" block */
  def loop: Parser[Any] = "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case e ~ _ ~ b =>  "while" +"(" +e+ ")" +b }

  /** block ::= "{" statement* "}" */
  def block: Parser[Any] = "{" ~> (statement) <~ "}" ^^ { case s => "{" + (s) + "}"}

}