package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string3)
  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string4)
  val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string5)
  val parsedExpr6 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string6)

  test("parser works 1") { assert(parsedExpr.get === complex1) }
  test("parser works 2") { assert(parsedExpr2.get === complex1) }
  test("parser works 3 Statement") { assert(parsedExpr3.get === complex3) }
  //test("parser works 4 Assignment") { assert(parsedExpr4.get === complex4) }
  //test("parser works 5 Conditional") { assert(parsedExpr5.get === complex5) }
  //test("parser works 6 Loop") { assert(parsedExpr6.get === complex6) }
}
