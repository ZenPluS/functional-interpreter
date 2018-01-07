open Interpreter 

let tuple_sum_test = LetRec ("sum", "t", 
                             IfThenElse (Equals (Ide "t", ETuple (Nil)), 
                                         EInt 0, 
                                         Sum(Head (Ide "t"), FunCall (Ide "sum", Tail (Ide "t")))),
                             FunCall (Ide "sum", ETuple (Seq (EInt 1, Seq(EInt 2, Seq(EInt 3, Nil))))));;

let pipe_test = Let ("square", Fun("x", Times(Ide "x", Ide "x")),
                     Let ("inc", Fun("x", Sum(Ide "x", EInt 1)),
                          EPipe (Seq(Ide "square", Seq(Ide "inc", Nil)))));;

let piping_test = Let ("square", Fun("x", Times(Ide "x", Ide "x")),
                       Let ("inc", Fun("x", Sum(Ide "x", EInt 1)),
                            FunCall (pipe_test, EInt 10)));;

let bin_power_test = Let ("times2", Fun("x", Times(Ide "x", EInt 2)),
                          FunCall (ManyTimes (3, (Ide "times2")), EInt 1));;

let many_pipes_test = FunCall (ManyTimes (2, pipe_test), EInt 10);;

let countdown_tuple_test n = LetRec ("count", "n",
                                     IfThenElse (Equals (Ide "n", EInt 0),
                                                 ETuple Nil,
                                                 Cons (Ide "n", FunCall (Ide "count", Diff(Ide "n", EInt 1)))),
                                     FunCall (Ide "count", EInt n));;
