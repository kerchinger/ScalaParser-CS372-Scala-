package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast._
import edu.luc.cs.laufer.cs473.expressions.evaluate.Cell

import scala.collection.mutable.Map
import scala.util.{Failure, Success, Try}
object evaluate {

  import ast._

  case class Cell(var value: Value) {
    def get: Value = value

    def set(value: Value): Unit = this.value = value
  }

  object Cell {
    def apply(i: Int): Cell = Cell(Left(i))
    def apply(i: List[Expr]) = 0
    val NULL = Cell(0)
  }

  type Instance = Map[String, Cell]

  type Store = Instance

  type Result = Try[Cell]

  type Value = Either[Int, Instance]

  def lookup(store: Store)(name: String): Result =
    store.get(name).fold {
      Failure(new NoSuchFieldException(name)): Result
    } {
      Success(_)
    }

  //case class Num(value: Int)  extends Value

  def binOp(store: Store, left: Expr, right: Expr, op: (Int, Int) => Int): Result = {
    for {Cell(Left(l)) <- apply(store)(left); Cell(Left(r)) <- apply(store)(right)} yield Cell(Left(op(l, r)))
  }

  def apply(store: Store)(s: Expr): Result = s match {
    case Constant(value) => {
      Success(Cell(value))
    }
    case Plus(left, right) => binOp(store, left, right, _ + _)
    case Minus(left, right) => {
      binOp(store, left, right, _ - _)
    }
    case Times(left, right) => {
      binOp(store, left, right, _ * _)
    }
    case Div(left, right) => {
      binOp(store, left, right, _ / _)
    }
    case Mod(left, right) => {
      binOp(store, left, right, _ % _)
    }
    case UMinus(expr) => {
      for {Cell(Left(e)) <- apply(store)(expr)} yield Cell(Left(-e))
    }
    case Variable(name) => {
      lookup(store)(name)
    }

    case Assign(left, right) =>
      val rValue = apply(store)(right)

      if (store.contains(left.asInstanceOf[Select].toString)) {
        val lValue = apply(store)(left)
        lValue.get.set(rValue.get.get)
      }
      else {
        store.put(PrettyPrinter.toFormattedString(left), rValue.get) // PrettyPrinter should not be here

        //store.put(apply(store)(left.asInstanceOf[Select].expr).toString + apply(store)(left.asInstanceOf[Select]).toString, rValue.get)
       // store.getOrElseUpdate(left.toString, rValue.get)
      }
      Success(Cell.NULL)

    case Cond(guard, thenBranch, elseBranch) => {
      apply(store)(guard) match {
        case Success(Cell.NULL) => apply(store)(elseBranch)
        case Success(_) => apply(store)(thenBranch)
        case f@Failure(_) => f
      }
    }
    case Block(expressions@_*) =>
      def doSequence: Result = {
        val i = expressions.iterator
        var result: Cell = Cell.NULL
        while (i.hasNext) {
          apply(store)(i.next()) match {
            case Success(r) => result = r
            case f@Failure(_) => return f
          }
        }
        Success(result)
      }
    {
      doSequence
    }
    case Loop(guard, body) =>
      def doLoop: Result = {
        var gValue = apply(store)(guard)
        while (gValue.get.value != Left(0)) {
          apply(store)(guard) match {
            case Success(Cell.NULL) => return Success(Cell.NULL)
            case Success(v) => apply(store)(body)
            case f@Failure(_) => return f
          }
        }
        Success(Cell.NULL)
      }
    {
      doLoop
    }

    case Struct(fields ) =>
      // Cell(Right(Map(fields.map(field => (field, Cell(0))): _*)))

    val i = fields.iterator
      while(i.hasNext){
        //apply(store)((Map(i.next()._1 -> i.next()._2).asInstanceOf[Expr]))
          //i.next()._1 -> i.next()._2.asInstanceOf[Expr])
        Map(i.next()._1 -> apply(store)(i.next().asInstanceOf[Expr]))
      }
       Success(Cell.NULL)

    case Select(record, field) => {
      val i = field.iterator
      var result: Cell = Cell.NULL
      while (i.hasNext) {
        apply(store)(record).get.get.right.get.apply(i.next().toString)
        }
      Success(Cell.NULL)
    }
  }
}

object behaviors {

  type Value = Cell
  type Store = Map[String, Cell]
  val result: Value = Cell.NULL

  def execute(store: Store)(e: Seq[_]): Cell = {
    result.set(Left(0))
    if(e.nonEmpty) {
      for (exp <- e) {
        result.set(Try(evaluate(store)(exp.asInstanceOf[Expr])).get.get.value)
      }
    }
    result
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "


  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c) => prefix + c.toString
    case UMinus(r) => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r) => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r) => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r) => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r) => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r) => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))

    case Variable(s) => prefix + s.toString

    case b: Block => buildBlockExprString(prefix, toFormattedStrings(prefix )(b.statements ))
    case Cond(l, r, x) => buildCondExprString(prefix, toFormattedString(prefix )(l),
      toFormattedString(prefix)(r), toFormattedString(prefix )(x))
    case Loop(l, r) => buildLoopExprString(prefix, toFormattedString(prefix)(l), toFormattedString(prefix )(r))
    case Assign(l, r) => buildExprString(prefix, "Assign", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Struct(m) => buildStructExprString(prefix, toFormattedStrings2(prefix)(m))
    case Select(l, r) => buildExprString(prefix, "Select", toFormattedString(prefix)(l), toFormattedStrings(prefix)(r))
  }


  def toFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def toPrettyFormattedString(e: Expr): String = toFormattedString("")(e)

  def toPrettyFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)

  def toFormattedStrings(prefix: String)(e: Seq[_]): String = {
    val result = new StringBuilder(prefix)
    if (e.nonEmpty) {
      for (exp <- e) {
        result.append(toFormattedString(prefix)(exp.asInstanceOf[Expr]))
        result.append(EOL)
      }
    }
    result.toString()
  }

  def toFormattedStrings2(prefix: String)(e: collection.Map[String,Expr]): String = {
    val result = new StringBuilder(prefix)
    if (e.nonEmpty) {
      for ((k,v) <- e) {
        result.append(k)
        result.append(" : ")
        result.append(v.asInstanceOf[Expr])
        result.append(EOL)
      }
    }
    result.toString()
  }


  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  def buildBlockExprString(prefix: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    if (exprString.trim.length > 0) {
      result.append("{")
      result.append(EOL)
      result.append(exprString.lines.map(s => INDENT + s).mkString(EOL))
      result.append(EOL)
      result.append("}")
    }
    result.toString()
  }

  def buildLoopExprString(prefix: String, exprString: String, blockString: String) = {
    val result = new StringBuilder(prefix)
    result.append("while (")
    result.append(exprString)
    result.append(") ")
    result.append(blockString)
    result.toString()
  }

  def buildCondExprString(prefix: String, ifString: String, blockString: String, elseBlock: String) = {
    val result = new StringBuilder(prefix)
    result.append("if (")
    result.append(ifString.dropRight(1))
    result.append(") ")
    result.append(blockString)
    if (elseBlock.trim.length > 0) {
      result.append(" else ")
      result.append(elseBlock)
    }
    result.toString()
  }

  def buildStructExprString(prefix: String, m: String) = {
    val result = new StringBuilder(prefix)
    if (m.trim.length > 0) {
      result.append("{")
      result.append(EOL)
      result.append(m.lines.map(a => INDENT + a).mkString(EOL))
      result.append(EOL)
      result.append("}")
    }
    result.toString()
  }
}
