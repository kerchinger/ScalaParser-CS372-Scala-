package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.TestFixtures._
import edu.luc.cs.laufer.cs473.expressions.behaviors._
import edu.luc.cs.laufer.cs473.expressions.evaluate.Cell
import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.collection.mutable.Map
import scala.util.{Success, Try}



object Main extends App {
  type Store = Map[String, Cell]
  type Value = Cell
  val store: Store = Map.empty

  println("p = " + complex1)
  println(toFormattedString(complex2))
  println("evaluate(p) = " + execute(store.asInstanceOf[behaviors.Store])(complex1string2))
  println()

  store.clear()

  println("q = " + complex2)
  println(toFormattedString(complex2))
  println("evaluate(q) = " + execute(store.asInstanceOf[behaviors.Store])(complex1string3))
}
class TestEvaluate extends FunSuite with BeforeAndAfter {

  type Store = Map[String, Cell]
  type Value = Cell
  type Result = Try[Value]
  val store: Store = Map.empty

  before {
    store.clear()
  }

  test("evaluate expr1") {
    assert(Try(evaluate(store)(parsed11)) === Success(Success(Cell(0).asInstanceOf[Value])))
    assert(store.size === 1)
    assert(store("x") === Cell(5))
  }

  test("evaluate expr2") {
    assert(Try(evaluate(store)(parsed22)) === Success(Success(Cell(0).asInstanceOf[Value])))
    assert(store.size === 2)
    assert(store("x") === Cell(5))
    assert(store("y") === Cell(7))
  }

  test("evaluate expr3") {
    assert(Try(evaluate(store)(parsed33)) === Success(Success(Cell(-4).asInstanceOf[Value])))
    assert(store.size === 2)
    assert(store("y2") === Cell(6))
    assert(store("y4") === Cell(9))
  }

  test("evaluate expr4") {
    assert(Try(evaluate(store)(parsed44)) === Success(Success(Cell(0).asInstanceOf[Value])))
    assert(store.size === 2)
    assert(store("y2") === Cell(6))
    assert(store("y4") === Cell(-4))
  }

  test("evaluate expr5") {
    assert(Try(evaluate(store)(parsed55)) === Success(Success(Cell(0).asInstanceOf[Value])))
    assert(store.size === 1)
    assert(store("x") === Cell(2))
  }

  test("evaluate expr6") {
    assert(Try(evaluate(store)(parsed66)) === Success(Success(Cell(0).asInstanceOf[Value])))
    assert(store.size === 2)
    assert(store("y") === Cell(0))
  }

}