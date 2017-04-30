package edu.luc.cs.laufer.cs473.expressions

import scala.language.postfixOps

object TestFixtures {

  import ast._
  val EOL = scala.util.Properties.lineSeparator

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    );

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex1string2 = "  ((1 + 2) - (3 * 4)) / 5  "

  val complex2 =
    Mod(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          UMinus(
            Constant(3)
          ),
          Constant(4)
        )
      ),
      Constant(5)
    )

  //* Test for statement */
  val complex1string3 = "{5+5; 5+x;}"
  val complex3 =
    Block(
      Plus(Constant(5), Constant(5)), Plus(Constant(5), Variable("x"))

    )

  //* Test for assignment*/
  val complex1string4 = "y = 5+2;"
  val complex4 =
    Assign(Seq(Variable("y")), Plus(Constant(5), Constant(2)))

  //* Test for Conditional */
  val complex1string5 = "if (0) { r = 5+2; } else { y = 5-1; }"
  val complex5 =
    Cond(
      Constant(0),
      Block(
        Assign(Seq(Variable("r")), Plus(Constant(5), Constant(2)))),
      Block(
        Assign(Seq(Variable("y")), Minus(Constant(5), Constant(1)))))

  //* Test for Loop */

  val complex1string6 = "while (y) { r = 5+2 ; y = 3-1; }"
  val complex6 =
    Loop(
      Variable("y"),
      Block(
        Assign(Seq(Variable("r")), Plus(Constant(5), Constant(2))),
        Assign(Seq(Variable("y")), Minus(Constant(3), Constant(1)))))

  val complex1string7 = "if (0) { r = 5+2;}"
  val complex7 =
    Cond(
      Constant(0),
      Block(
        Assign(Seq(Variable("r")), Plus(Constant(5), Constant(2)))),
      Block())
  // Test for field

  // Test for struct
  //x = { a: 3 + 4, b: 5 + 6 };
  //list = { head: 1, tail: { head: 2, tail: { head: 3, tail: 0 } } };


  // Test cases for the unparser
  val parsed1: Seq[Expr] = Seq(Assign(Seq(Variable("x")), Constant(5)))
  val parsed2: Seq[Expr] = Seq(Assign(Seq(Variable("x")), Constant(5)), Assign(Seq(Variable("y")), Constant(7)))
  val parsed3: Seq[Expr] = Seq(Assign(Seq(Variable("y2")), Constant(6)), Assign(Seq(Variable("y4")), Constant(9)), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5)))
  val parsed4: Seq[Expr] = Seq(Assign(Seq(Variable("y2")), Constant(6)), Assign(Seq(Variable("y4")), Constant(9)), Assign(Seq(Variable("y4")), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5))))
  val parsed5: Seq[Expr] = Seq(Cond( Constant(9), Block(Assign(Seq(Variable("x")), Constant(2))),
    Block(Assign(Seq(Variable("x")), Constant(3)))))
  Assign(Seq(Variable("y")), Plus(Variable("y"), Constant(1)));
  val parsed6: Seq[Expr] = Seq(Loop(Variable("y"), Block(Assign(Seq(Variable("r")), Plus(Constant(5), Constant(2))), Assign(Seq(Variable("y")), Minus(Constant(3), Constant(1))))))
  val unparsed1 = "x = 5;" + EOL
  val unparsed2 = "x = 5;" + EOL + "y = 7;" + EOL
  val unparsed3 = "y2 = 6;" + EOL + "y4 = 9;" + EOL + "(((1 + y2) - (3 * y4)) / 5)" + EOL
  val unparsed4 = "y2 = 6;" + EOL + "y4 = 9;" + EOL + "y4 = (((1 + y2) - (3 * y4)) / 5);" + EOL
  val unparsed5 = "if (9) {" + EOL + "  x = 2;" + EOL + "} else {" + EOL + "  x = 3;" + EOL + "}" + EOL
  val unparsed6 = "while (y) {" + EOL + "  r = (5 + 2);" + EOL + "  y = (3 - 1);" + EOL + "}" + EOL

//TEST cases for evaluater
  val parsed11 = Assign(Seq(Variable("x")), Constant(5))
  val parsed22 = Block(Assign(Seq(Variable("x")), Constant(5)), Assign(Seq(Variable("y")), Constant(7)))
  val parsed33 = Block(Assign(Seq(Variable("y2")), Constant(6)), Assign(Seq(Variable("y4")), Constant(9)), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5)))
  val parsed44= Block(Assign(Seq(Variable("y2")), Constant(6)), Assign(Seq(Variable("y4")), Constant(9)), Assign(Seq(Variable("y4")), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5))))
  val parsed55= Cond(Constant(9), Block(Assign(Seq(Variable("x")), Constant(2))),
    Block(Assign(Seq(Variable("x")), Constant(3))))
  Assign(Seq(Variable("y")), Plus(Variable("y"), Constant(1)));
  val parsed66 = Block(Assign(Seq(Variable("y")), Constant(2)), Loop(Variable("y"), Block(Assign(Seq(Variable("r")), Plus(Constant(5), Constant(2))), Assign(Seq(Variable("y")), Minus(Variable("y"), Constant(1))))))
}
