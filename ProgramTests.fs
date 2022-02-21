module ProgramTests

open Program

// Preparation
let a1 = N 42
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

// Exercise 3.1
printf "arithEvalSimple a1 : %A\n" (arithEvalSimple a1)
printf "arithEvalSimple a2 : %A\n" (arithEvalSimple a2)
printf "arithEvalSimple a3 : %A\n" (arithEvalSimple a3)
printf "arithEvalSimple a4 : %A\n" (arithEvalSimple a4)
printf "arithEvalSimple a5 : %A\n" (arithEvalSimple a5)

// Exercise 3.2
printf "arithEvalState a6 (Map.ofList [ (\"x\", 5) ]) : %A\n" (arithEvalState a6 (Map.ofList [ ("x", 5) ]))
printf "arithEvalState a6 (Map.ofList [ (\"y\", 5) ]) : %A\n" (arithEvalState a6 (Map.ofList [ ("y", 5) ]))

printf
    "arithEvalState a7 (Map.ofList [ (\"x\", 4); (\"y\", 5) ]) : %A\n"
    (arithEvalState a7 (Map.ofList [ ("x", 4); ("y", 5) ]))

printf
    "arithEvalState a7 (Map.ofList [ (\"y\", 4); (\"z\", 5) ]) : %A\n"
    (arithEvalState a7 (Map.ofList [ ("y", 4); ("z", 5) ]))


// Exercise 3.3
printf "arithEval WL [] Map.empty : %A\n" (arithEval WL [] Map.empty)
printf "arithEval WL hello Map.empty : %A\n" (arithEval WL hello Map.empty)
printf "arithEval (PV(N 0)) hello Map.empty : %A\n" (arithEval (PV(N 0)) hello Map.empty)

printf
    "arithEval arithSingleLetterScore hello (Map.ofList [ (\"_pos_\", 4); (\"_acc_\", 0) ]) : %A\n"
    (arithEval
        arithSingleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 0) ]))

printf
    "arithEval arithSingleLetterScore hello (Map.ofList [ (\"_pos_\", 4); (\"_acc_\", 42) ]) : %A\n"
    (arithEval
        arithSingleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 42) ]))

printf
    "arithEval arithDoubleLetterScore hello (Map.ofList [ (\"_pos_\", 4); (\"_acc_\", 0) ]) : %A\n"
    (arithEval
        arithDoubleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 0) ]))

printf
    "arithEval arithDoubleLetterScore hello (Map.ofList [ (\"_pos_\", 4); (\"_acc_\", 42) ]) : %A\n"
    (arithEval
        arithDoubleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 42) ]))

printf
    "arithEval arithTripleLetterScore hello (Map.ofList [ (\"_pos_\", 4); (\"_acc_\", 0) ]) : %A\n"
    (arithEval
        arithTripleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 0) ]))

printf
    "arithEval arithTripleLetterScore hello (Map.ofList [ (\"_pos_\", 4); (\"_acc_\", 42) ]) : %A\n"
    (arithEval
        arithTripleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 42) ]))


// Exercise 3.4
printf "charEval (C 'H') [] Map.empty : %A\n" (charEval (C 'H') [] Map.empty)
printf "charEval (ToLower(CV(N 0))) hello Map.empty : %A\n" (charEval (ToLower(CV(N 0))) hello Map.empty)
printf "charEval (ToUpper(C 'h')) [] Map.empty : %A\n" (charEval (ToUpper(C 'h')) [] Map.empty)
printf "charEval (ToLower(C '*')) [] Map.empty : %A\n" (charEval (ToLower(C '*')) [] Map.empty)

printf
    "charEval (CV(V \"x\" .-. N 1)) hello (Map.ofList [ (\"x\", 5) ]) : %A\n"
    (charEval (CV(V "x" .-. N 1)) hello (Map.ofList [ ("x", 5) ]))

// Exercise 3.5
printf "boolEval TT [] Map.empty : %A\n" (boolEval TT [] Map.empty)
printf "boolEval FF [] Map.empty : %A\n" (boolEval FF [] Map.empty)

printf
    "boolEval ((V \"x\" .+. V \"y\") .=. (V \"y\" .+. V \"x\")) [] (Map.ofList [ (\"x\", 5); (\"y\", 7) ]) : %A\n"
    (boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [ ("x", 5); ("y", 7) ]))

printf
    "boolEval ((V \"x\" .+. V \"y\") .=. (V \"y\" .-. V \"x\")) [] (Map.ofList [ (\"x\", 5); (\"y\", 7) ]) : %A\n"
    (boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [ ("x", 5); ("y", 7) ]))

printf
    "boolEval (IsLetter (CV(V \"x\"))) hello (Map.ofList [ (\"x\", 4) ]) : %A\n"
    (boolEval (IsLetter(CV(V "x"))) hello (Map.ofList [ ("x", 4) ]))

printf
    "boolEval (IsLetter (CV(V \"x\"))) (('1', 0) :: hello) (Map.ofList [ (\"x\", 0) ]) : %A\n"
    (boolEval (IsLetter(CV(V "x"))) (('1', 0) :: hello) (Map.ofList [ ("x", 0) ]))

printf
    "boolEval (IsDigit (CV(V \"x\"))) hello (Map.ofList [ (\"x\", 4)] ) : %A\n"
    (boolEval (IsDigit(CV(V "x"))) hello (Map.ofList [ ("x", 4) ]))

printf
    "boolEval (IsDigit (CV(V \"x\"))) (('1', 0) :: hello) (Map.ofList [ (\"x\", 0) ]) : %A\n"
    (boolEval (IsDigit(CV(V "x"))) (('1', 0) :: hello) (Map.ofList [ ("x", 0) ]))

// Exercise 3.6
printf "boolEval (isConsonant (C 'H')) [] Map.empty : %A\n" (boolEval (isConsonant (C 'H')) [] Map.empty)
printf "boolEval (isConsonant (C 'h')) [] Map.empty : %A\n" (boolEval (isConsonant (C 'h')) [] Map.empty)
printf "boolEval (isConsonant (C 'A')) [] Map.empty : %A\n" (boolEval (isConsonant (C 'A')) [] Map.empty)

printf
    "boolEval (isConsonant (CV(V \"x\"))) hello (Map.ofList [ (\"x\", 0) ]) %A\n"
    (boolEval (isConsonant (CV(V "x"))) hello (Map.ofList [ ("x", 0) ]))

printf
    "boolEval (isConsonant (CV(V \"x\"))) hello (Map.ofList [ (\"x\", 0) ]) %A\n"
    (boolEval (isConsonant (CV(V "x"))) hello (Map.ofList [ ("x", 1) ]))

// Exercise 3.7
printf "evalStmnt Skip [] Map.empty : %A\n" (evalStmnt Skip [] Map.empty)
printf "evalStmnt (Ass(\"x\", N 5)) [] Map.empty : %A\n" (evalStmnt (Ass("x", N 5)) [] Map.empty)

printf
    "evalStmnt (Seq(Ass(\"x\", WL), Ass(\"y\", N 7))) hello Map.empty : %A\n"
    (evalStmnt (Seq(Ass("x", WL), Ass("y", N 7))) hello Map.empty)

printf
    "evalStmnt (ITE(WL .>=. N 5, Ass(\"x\", N 1), Ass(\"x\", N 2))) hello Map.empty : %A\n"
    (evalStmnt (ITE(WL .>=. N 5, Ass("x", N 1), Ass("x", N 2))) hello Map.empty)

printf
    "evalStmnt (ITE(WL .<. N 5, Ass(\"x\", N 1), Ass (\"x\", N 2))) hello Map.empty : %A\n"
    (evalStmnt (ITE(WL .<. N 5, Ass("x", N 1), Ass("x", N 2))) hello Map.empty)

printf
    "evalStmnt (While(V \"x\" .<=. WL, Seq(Ass(\"y\", V \"y\" .+. V \"x\"), Ass(\"x\", V \"x\" .+. N 1)))) hello Map.empty : %A\n"
    (evalStmnt (While(V "x" .<=. WL, Seq(Ass("y", V "y" .+. V "x"), Ass("x", V "x" .+. N 1)))) hello Map.empty)

printf
    "evalStmnt (While(V \"x\" .<=. WL, Seq(Ass(\"y\", V \"y\" .+. V \"x\"), Ass(\"x\", V \"x\" .+. N 1)))) hello (Map.ofList [ (\"x\", 3); (\"y\", 100) ]) : %A\n"
    (evalStmnt
        (While(V "x" .<=. WL, Seq(Ass("y", V "y" .+. V "x"), Ass("x", V "x" .+. N 1))))
        hello
        (Map.ofList [ ("x", 3); ("y", 100) ]))

// Exercise 3.8
let containsNumbers =
    stmntToSquareFun (
        Seq(
            Ass("_result_", V "_acc_"),
            While(
                V "i" .<. WL,
                ITE(
                    IsDigit(CV(V "i")),
                    Seq(Ass("_result_", V "_result_" .*. N -1), Ass("i", WL)),
                    Ass("i", V "i" .+. N 1)
                )
            )
        )
    )

printf "singleLetterScore hello 0 0 : %A\n" (singleLetterScore hello 0 0)
printf "doubleLetterScore hello 0 0 : %A\n" (doubleLetterScore hello 0 0)
printf "tripleLetterScore hello 0 0 : %A\n" (tripleLetterScore hello 0 0)
printf "singleLetterScore hello 0 42 : %A\n" (singleLetterScore hello 0 42)
printf "doubleLetterScore hello 0 42 : %A\n" (doubleLetterScore hello 0 42)
printf "tripleLetterScore hello 0 42 : %A\n" (tripleLetterScore hello 0 42)
printf "containsNumbers hello 5 50 : %A\n" (containsNumbers hello 5 50)
printf "containsNumbers (('0', 100) :: hello) 5 50 : %A\n" (containsNumbers (('0', 100) :: hello) 5 50)
printf "containsNumbers (hello @ [ ('0', 100) ]) 5 50 : %A\n" (containsNumbers (hello @ [ ('0', 100) ]) 5 50)

// Exercise 3.9
let helly: word =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('Y', 1) ]

(*
    Create a square function oddConsonants of type squareFun that negates the accumulator if there are an
    odd number of consonants in the word placed over the square.
*)

printf "stmntToSquareFun oddConsonants hello 0 1 : %A\n" (stmntToSquareFun oddConsonants hello 0 1) // should be -1
printf "stmntToSquareFun oddConsonants helly 0 1 : %A\n" (stmntToSquareFun oddConsonants helly 0 1) // should be 1

// Exercise 3.10
printf "calculatePoints2 [ DLS; SLS; TLS; SLS; DWS ] hello : %A\n" (calculatePoints2 [ DLS; SLS; TLS; SLS; DWS ] hello)
printf "calculatePoints2 [ DLS; DWS; TLS; TWS; DWS ] hello : %A\n" (calculatePoints2 [ DLS; DWS; TLS; TWS; DWS ] hello)
