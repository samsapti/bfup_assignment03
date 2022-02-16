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

let hello: word =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

// Exercise 3.1
printf "arithEvalSimple a1 : %A\n" (arithEvalSimple a1)
printf "arithEvalSimple a2 : %A\n" (arithEvalSimple a2)
printf "arithEvalSimple a3 : %A\n" (arithEvalSimple a3)
printf "arithEvalSimple a4 : %A\n" (arithEvalSimple a4)
printf "arithEvalSimple a5 : %A\n" (arithEvalSimple a5)

// Exercise 3.2
printf "arithEvalState a6 (Map.ofList [(\"x\", 5)]) : %A\n" (arithEvalState a6 (Map.ofList [ ("x", 5) ]))
printf "arithEvalState a6 (Map.ofList [(\"y\", 5)]) : %A\n" (arithEvalState a6 (Map.ofList [ ("y", 5) ]))

printf
    "arithEvalState a7 (Map.ofList [(\"x\", 4); (\"y\", 5)]) : %A\n"
    (arithEvalState a7 (Map.ofList [ ("x", 4); ("y", 5) ]))

printf
    "arithEvalState a7 (Map.ofList [(\"y\", 4); (\"z\", 5)]) : %A\n"
    (arithEvalState a7 (Map.ofList [ ("y", 4); ("z", 5) ]))


// Exercise 3.3
printf "arithEval WL [] Map.empty : %A\n" (arithEval WL [] Map.empty)
printf "arithEval WL hello Map.empty : %A\n" (arithEval WL hello Map.empty)
printf "arithEval (PV (N 0)) hello Map.empty : %A\n" (arithEval (PV(N 0)) hello Map.empty)

printf
    "arithEval arithSingleLetterScore hello (Map.ofList [(\"_pos_\", 4); (\"_acc_\", 0)]) : %A\n"
    (arithEval
        arithSingleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 0) ]))

printf
    "arithEval arithSingleLetterScore hello (Map.ofList [(\"_pos_\", 4); (\"_acc_\", 42)]) : %A\n"
    (arithEval
        arithSingleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 42) ]))

printf
    "arithEval arithDoubleLetterScore hello (Map.ofList [(\"_pos_\", 4); (\"_acc_\", 0)]) : %A\n"
    (arithEval
        arithDoubleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 0) ]))

printf
    "arithEval arithDoubleLetterScore hello (Map.ofList [(\"_pos_\", 4); (\"_acc_\", 42)]) : %A\n"
    (arithEval
        arithDoubleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 42) ]))

printf
    "arithEval arithTripleLetterScore hello (Map.ofList [(\"_pos_\", 4); (\"_acc_\", 0)]) : %A\n"
    (arithEval
        arithTripleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 0) ]))

printf
    "arithEval arithTripleLetterScore hello (Map.ofList [(\"_pos_\", 4); (\"_acc_\", 42)]) : %A\n"
    (arithEval
        arithTripleLetterScore
        hello
        (Map.ofList [ ("_pos_", 4)
                      ("_acc_", 42) ]))


// Exercise 3.4
printf "charEval (C 'H') [] Map.empty : %A\n" (charEval (C 'H') [] Map.empty)
printf "charEval (ToLower (CV (N 0))) hello Map.empty : %A\n" (charEval (ToLower(CV(N 0))) hello Map.empty)
printf "charEval (ToUpper (C 'h')) [] Map.empty : %A\n" (charEval (ToUpper(C 'h')) [] Map.empty)
printf "charEval (ToLower (C '*')) [] Map.empty : %A\n" (charEval (ToLower(C '*')) [] Map.empty)

printf
    "charEval (CV (V \"x\" .-. N 1)) hello (Map.ofList [(\"x\", 5)]) : %A\n"
    (charEval (CV(V "x" .-. N 1)) hello (Map.ofList [ ("x", 5) ]))

// Exercise 3.5
printf "boolEval TT [] Map.empty : %A\n" (boolEval TT [] Map.empty)
printf "boolEval FF [] Map.empty : %A\n" (boolEval FF [] Map.empty)

printf
    "boolEval ((V \"x\" .+. V \"y\") .=. (V \"y\" .+. V \"x\")) [] (Map.ofList [(\"x\", 5); (\"y\", 7)]) : %A\n"
    (boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [ ("x", 5); ("y", 7) ]))

printf
    "boolEval ((V \"x\" .+. V \"y\") .=. (V \"y\" .-. V \"x\")) [] (Map.ofList [(\"x\", 5); (\"y\", 7)]) : %A\n"
    (boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [ ("x", 5); ("y", 7) ]))

printf
    "boolEval (IsLetter (CV (V \"x\"))) hello (Map.ofList [(\"x\", 4)]) : %A\n"
    (boolEval (IsLetter(CV(V "x"))) hello (Map.ofList [ ("x", 4) ]))

printf
    "boolEval (IsLetter (CV (V \"x\"))) (('1', 0)::hello) (Map.ofList [(\"x\", 0)]) : %A\n"
    (boolEval (IsLetter(CV(V "x"))) (('1', 0) :: hello) (Map.ofList [ ("x", 0) ]))

printf
    "boolEval (IsDigit (CV (V \"x\"))) hello (Map.ofList [(\"x\", 4)]) : %A\n"
    (boolEval (IsDigit(CV(V "x"))) hello (Map.ofList [ ("x", 4) ]))