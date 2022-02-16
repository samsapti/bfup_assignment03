(*
    Functional Programming - Assignment 2
    Sam Al-Sapti (sals@itu.dk)
    February 12th, 2022
*)


module Program

// Preparation
type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

type cExp =
    | C of char
    | ToUpper of cExp
    | ToLower of cExp
    | CV of aExp

type bExp =
    | TT
    | FF
    | AEq of aExp * aExp
    | ALt of aExp * aExp
    | Not of bExp
    | Conj of bExp * bExp
    | IsDigit of cExp
    | IsLetter of cExp
    | IsVowel of cExp

type word = (char * int) list

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)
let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
let (.>=.) a b = ~~(a .<. b)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b)

let arithSingleLetterScore = PV(V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"


(*
    GREEN
*)

// Exercise 3.1
let rec arithEvalSimple =
    function
    | N a -> a
    | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a, b) -> arithEvalSimple a * arithEvalSimple b
    | V _ -> failwith "Not Implemented"
    | WL -> failwith "Not Implemented"
    | PV _ -> failwith "Not Implemented"

// Exercise 3.2
let rec arithEvalState exp s =
    match exp with
    | N a -> a
    | V a -> Map.tryFind a s |> Option.defaultValue 0
    | Add (a, b) -> arithEvalState a s + arithEvalState b s
    | Sub (a, b) -> arithEvalState a s - arithEvalState b s
    | Mul (a, b) -> arithEvalState a s * arithEvalState b s
    | WL -> failwith "Not Implemented"
    | PV _ -> failwith "Not Implemented"

// Exercise 3.3
let rec arithEval exp (w: word) s =
    match exp with
    | N a -> a
    | V a -> Map.tryFind a s |> Option.defaultValue 0
    | WL -> List.length w
    | PV a -> snd w.[arithEval a w s]
    | Add (a, b) -> arithEval a w s + arithEval b w s
    | Sub (a, b) -> arithEval a w s - arithEval b w s
    | Mul (a, b) -> arithEval a w s * arithEval b w s

// Exercise 3.4
let rec charEval exp (w: word) s =
    match exp with
    | C c -> c
    | ToUpper c -> System.Char.ToUpper(charEval c w s)
    | ToLower c -> System.Char.ToLower(charEval c w s)
    | CV a -> fst w.[arithEval a w s]

// Exercise 3.5
let rec boolEval exp w s =
    let isVowel c =
        (System.Char.ToLower >> List.contains) c [ 'a'; 'e'; 'i'; 'o'; 'u'; 'y' ]

    let eval f g x y = f (g x w s) (g y w s)

    match exp with
    | TT -> true
    | FF -> false
    | AEq (x, y) -> eval (=) arithEval x y
    | ALt (x, y) -> eval (<) arithEval x y
    | Not b -> boolEval b w s |> not
    | Conj (b1, b2) -> eval (&&) boolEval b1 b2
    | IsDigit c -> charEval c w s |> System.Char.IsDigit
    | IsLetter c -> charEval c w s |> System.Char.IsLetter
    | IsVowel c -> charEval c w s |> isVowel

