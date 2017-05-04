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
    Block(Plus(Constant(5),Constant(5)), Plus(Constant(5),Select(Variable("x"),List())))

  //* Test for assignment*/
  val complex1string4 = "y = 5+2;"
  val complex4 =
    Assign(Select(Variable("y"),List()),Plus(Constant(5),Constant(2)))

  //* Test for Conditional */
  val complex1string5 = "if (0) { r = 5+2; } else { y = 5-1; }"
  val complex5 =
    Cond(Constant(0),Block((Assign(Select(Variable("r"),List()),Plus(Constant(5),Constant(2))))),Block((Assign(Select(Variable("y"),List()),Minus(Constant(5),Constant(1))))))

  //* Test for Loop */

  val complex1string6 = "while (y) { r = 5+2 ; y = 3-1; }"
  val complex6 =
    Loop(Select(Variable("y"),List()),Block(Assign(Select(Variable("r"),List()),Plus(Constant(5),Constant(2))), Assign(Select(Variable("y"),List()),Minus(Constant(3),Constant(1)))))

  val complex1string7 = "if (0) { r = 5+2;}"
  val complex7 =
    Cond(Constant(0),Block((Assign(Select(Variable("r"),List()),Plus(Constant(5),Constant(2))))),Block())
  // Test for field

  // Test for struct
  //x = { a: 3 + 4, b: 5 + 6 };
  val complex1string8 = "x = { a: 3 + 4, b: 5 + 6 };"
    val complex8 = Assign(Select(Variable("x"),List()),Select(Struct(Map("a" -> Plus(Constant(3),Constant(4)), "b" -> Plus(Constant(5),Constant(6)))),List()))

  //list = { head: 1, tail: { head: 2, tail: { head: 3, tail: 0 } } };
  val complexstring9 = "list = { head: 1, tail: { head: 2, tail: { head: 3, tail: 0 } } };"
  val complex9 = Assign(Select(Variable("list"),List()),Select(Struct(Map("head" -> Constant(1), "tail" -> Select(Struct(Map("head" -> Constant(2), "tail" -> Select(Struct(Map("head" -> Constant(3), "tail" -> Constant(0))),List()))),List()))),List()))
 // x.a; // evaluates to 7
  val complexstring10 = "x.a;"
  val complex10 = Select(Variable("x"),List(Variable("a")))
  //{ a: 3 + 4, b: 5 + 6 }.a; // also evaluates to 7
  val complexstring11 = "{ a: 3 + 4, b: 5 + 6 }.a;"
  val complex11 = Select(Struct(Map("a" -> Plus(Constant(3),Constant(4)), "b" -> Plus(Constant(5),Constant(6)))),List(Variable("a")))
  //list.tail.tail.head;
  val complexstring12 = "list.tail.tail.head;"
    val complex12 = Select(Variable("list"),List(Variable("tail"), Variable("tail"), Variable("head")))

  // Test cases for the unparser
  val parsed1: Seq[Expr] = Seq(Assign((Variable("x")), Constant(5)))
  val parsed2: Seq[Expr] = Seq(Assign((Variable("x")), Constant(5)), Assign((Variable("y")), Constant(7)))
  val parsed3: Seq[Expr] = Seq(Assign((Variable("y2")), Constant(6)), Assign((Variable("y4")), Constant(9)), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5)))
  val parsed4: Seq[Expr] = Seq(Assign((Variable("y2")), Constant(6)), Assign((Variable("y4")), Constant(9)), Assign((Variable("y4")), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5))))
  val parsed5: Seq[Expr] = Seq(Cond( Constant(9), Block(Assign((Variable("x")), Constant(2))),
    Block(Assign((Variable("x")), Constant(3)))))
  Assign((Variable("y")), Plus(Variable("y"), Constant(1)));
  val parsed6: Seq[Expr] = Seq(Loop(Variable("y"), Block(Assign((Variable("r")), Plus(Constant(5), Constant(2))), Assign((Variable("y")), Minus(Constant(3), Constant(1))))))
  val parsed7: Seq[Expr] = Seq(Assign(Select(Variable("x"),List()),Select(Struct(Map("a" -> Plus(Constant(3),Constant(4)), "b" -> Plus(Constant(5),Constant(6)))),List())))
  val parsed8: Seq[Expr] = Seq(Assign(Select(Variable("list"),List()),Select(Struct(Map("head" -> Constant(1), "tail" -> Select(Struct(Map("head" -> Constant(2), "tail" -> Select(Struct(Map("head" -> Constant(3), "tail" -> Constant(0))),List()))),List()))),List())))
  val parsed9: Seq[Expr] = Seq(Select(Variable("x"),List(Variable("a"))))
  val parsed10: Seq[Expr] = Seq(Select(Variable("list"),List(Variable("tail"), Variable("tail"), Variable("head"))))

  val unparsed1 = "x = 5;" + EOL
  val unparsed2 = "x = 5;" + EOL + "y = 7;" + EOL
  val unparsed3 = "y2 = 6;" + EOL + "y4 = 9;" + EOL + "(((1 + y2) - (3 * y4)) / 5)" + EOL
  val unparsed4 = "y2 = 6;" + EOL + "y4 = 9;" + EOL + "y4 = (((1 + y2) - (3 * y4)) / 5);" + EOL
  val unparsed5 = "if (9) {" + EOL + "  x = 2;" + EOL + "} else {" + EOL + "  x = 3;" + EOL + "}" + EOL
  val unparsed6 = "while (y) {" + EOL + "  r = (5 + 2);" + EOL + "  y = (3 - 1);" + EOL + "}" + EOL
  val unparsed7 =  "x = {" + EOL + "a : (3 + 4)" + EOL + "b : (5 + 6)" + EOL + EOL + "};" + EOL
  val unparsed8 = "list = {" + EOL +"head : 1" +EOL+ "tail : {" +EOL+ "head : 2" +EOL+ "tail : {" +EOL+ "head : 3" +EOL+ "tail : 0" + EOL + EOL + "}" + EOL + EOL + "}" + EOL + EOL + "};" + EOL
  val unparsed9 = "x.a" + EOL
  val unparsed10 = "list.tail" + EOL + ".tail" + EOL + ".head" + EOL


  //TEST cases for evaluater
  val parsed11 = Assign(Select(Variable("x"), List()), Constant(5))
  val parsed22 = Block(Assign(Select(Variable("x"), List()), Constant(5)), Assign(Select(Variable("y"),List()), Constant(7)))
  val parsed33 = Block(Assign(Select(Variable("y2"), List()), Constant(6)), Assign(Select(Variable("y4"),List()), Constant(9)), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5)))
  val parsed44= Block(Assign(Select(Variable("y2"),List()), Constant(6)), Assign(Select(Variable("y4"),List()), Constant(9)), Assign(Select(Variable("y4"),List()), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5))))
  val parsed55= Cond(Constant(9), Block(Assign(Select(Variable("x"),List()), Constant(2))),
    Block(Assign(Select(Variable("x"),List()), Constant(3))))
  Assign(Select(Variable("y"),List()), Plus(Variable("y"), Constant(1)));
  val parsed66 = Block(Assign(Select(Variable("y"),List()), Constant(2)), Loop(Variable("y"), Block(Assign(Select(Variable("r"),List()), Plus(Constant(5), Constant(2))), Assign(Select(Variable("y"),List()), Minus(Variable("y"), Constant(1))))))
}
