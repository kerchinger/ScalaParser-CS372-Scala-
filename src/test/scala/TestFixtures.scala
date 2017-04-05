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

  val complex1string3 = "{ r = 5+2 ; y = 5-1 }"
  val complex3 =
    Block(
      Assign("r", Plus(Constant(5), Variable("x"))),
      Assign("y", Minus(Constant(5), Constant(1)))
    );

  val complex1string4 = "if (4) { r = 5+2  } else { y = 5-1 }"
  val complex4 =
    Cond(
      Constant(4),
      Assign("r", Plus(Constant(5), Variable("x"))),
      Assign("y", Minus(Constant(5), Constant(1)))
    );

  val complex1string5 = "if (0) { r = 5+2 } else { y = 5-1 }"
  val complex5 =
    Cond(
      Constant(0),
      Assign("r", Plus(Constant(5), Variable("x"))),
      Assign("y", Minus(Constant(5), Constant(1)))
    );

  val complex1string6 = "while (y) { r = 5+2 ; y = 5-1 }"
  val complex6 =
    Loop(
      Variable("y"),
      Block(
        Assign("r",Plus(Constant(5), Variable("x"))),
        Assign("y", Minus(Constant(5), Constant(1)))
      )
    );
}
