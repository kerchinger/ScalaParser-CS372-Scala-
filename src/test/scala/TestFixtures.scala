package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._

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
    );

  //* Test for statement */
  val complex1string3 = "{5+5; 5+x;}"
  val complex3 =
    Block ( Plus(Constant(5), Constant(5)), Plus(Constant(5), Variable("x")))


  //* Test for assignment*/
  val complex1string4 = "y = 5+2;"
  val complex4 =
      Assign(Variable("y"), Plus(Constant(5), Constant(2)));

  //* Test for Conditional */
  val complex1string5 = "if (0) { r = 5+2; } else { y = 5-1; }"
  val complex5 =
    Cond(
      Constant(0),
      Assign(Variable("r"), Plus(Constant(5), Variable("x"))),
      Assign(Variable("y"), Minus(Constant(5), Constant(1)))
    );

  //* Test for Loop */
  val complex1string6 = "while (y) { r = 5+2 ; y = 3-1; }"
  val complex6 =
    Loop(
      Variable("y"),
      Block(
        Assign(Variable("r"),Plus(Constant(5), Variable("x"))),
        Assign(Variable("y"), Minus(Constant(3), Constant(1)))
      )
    );
}
