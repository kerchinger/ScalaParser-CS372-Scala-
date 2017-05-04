package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.TestFixtures._
import org.scalatest.FunSuite

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
}

class TestCombinatorParser extends FunSuite {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string3)
  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string4)
  val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string5)
  val parsedExpr6 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string6)
  val parsedExpr7 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string7)
  val parsedExpr8 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string8)
  val parsedExpr9 = CombinatorParser.parseAll(CombinatorParser.statement, complexstring9)
  val parsedExpr10 = CombinatorParser.parseAll(CombinatorParser.statement, complexstring10)
  val parsedExpr11= CombinatorParser.parseAll(CombinatorParser.statement, complexstring11)
  val parsedExpr12= CombinatorParser.parseAll(CombinatorParser.statement, complexstring12)



  test("parser works 1") {
    assert(parsedExpr.get === complex1)
  }
  test("parser works 2") {
    assert(parsedExpr2.get === complex1)
  }
  test("parser works 3 Statement") {
    assert(parsedExpr3.get === complex3)
  }
  test("parser works 4 Assignment") {
    assert(parsedExpr4.get === complex4)
  }
  test("parser works 5 Conditional") {
    assert(parsedExpr5.get === complex5)
  }
  test("parser works 6 Loop") {
    assert(parsedExpr6.get === complex6)
  }
  test("parser works 7 Cond #2") {
    assert(parsedExpr7.get === complex7)
  }
  test("parser works  8Cond #2") {
    assert(parsedExpr8.get === complex8)
  }
  test("parser works 9 ") {
    assert(parsedExpr9.get === complex9)
  }
  test("parser works 10 C") {
    assert(parsedExpr10.get === complex10)
  }
  test("parser works 11" ) {
    assert(parsedExpr11.get === complex11)
  }
  test("parser works 12" ) {
    assert(parsedExpr12.get === complex12)
  }
}

class TestUnParser extends  FunSuite {
  test("unparser works 1") { assert(PrettyPrinter.toFormattedString(parsed1) == unparsed1) }
  test("unparser works 2") { assert(PrettyPrinter.toFormattedString(parsed2) == unparsed2) }
  test("unparser works 3") { assert(PrettyPrinter.toFormattedString(parsed3) == unparsed3) }
  test("unparser works 4") { assert(PrettyPrinter.toFormattedString(parsed4) == unparsed4) }
  test("unparser works 5") { assert(PrettyPrinter.toFormattedString(parsed5) == unparsed5) }
  test("unparser works 6") { assert(PrettyPrinter.toFormattedString(parsed6) == unparsed6) }
  test("unparser works 7") { assert(PrettyPrinter.toFormattedString(parsed7) == unparsed7) }
  test("unparser works 8") { assert(PrettyPrinter.toFormattedString(parsed8) == unparsed8) }
  test("unparser works 9") { assert(PrettyPrinter.toFormattedString(parsed9) == unparsed9) }
  test("unparser works 10") { assert(PrettyPrinter.toFormattedString(parsed10) == unparsed10) }

}
