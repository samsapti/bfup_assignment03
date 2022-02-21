(*
    Functional Programming - Assignment 3
    Sam Al-Sapti (sals@itu.dk)
    February 16th, 2022
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
    | IfFunc of cExp * (char -> bool)

type stmnt =
    | Skip
    | Ass of string * aExp
    | Seq of stmnt * stmnt
    | ITE of bExp * stmnt * stmnt
    | While of bExp * stmnt

type word = (char * int) list
type squareFun = word -> int -> int -> int
type square = (int * squareFun) list
type square2 = (int * stmnt) list

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

let SLS: square2 = [ (0, Ass("_result_", arithSingleLetterScore)) ]
let DLS: square2 = [ (0, Ass("_result_", arithDoubleLetterScore)) ]
let TLS: square2 = [ (0, Ass("_result_", arithTripleLetterScore)) ]

let DWS: square2 =
    [ (1, Ass("_result_", arithDoubleWordScore)) ]
    @ SLS

let TWS: square2 =
    [ (1, Ass("_result_", arithTripleWordScore)) ]
    @ SLS

let hello: word =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]


(*
    GREEN
*)

// Exercise 3.1
let rec arithEvalSimple =
    function
    | N a0 -> a0
    | Add (x, y) -> arithEvalSimple x + arithEvalSimple y
    | Sub (x, y) -> arithEvalSimple x - arithEvalSimple y
    | Mul (x, y) -> arithEvalSimple x * arithEvalSimple y
    | V _ -> failwith "Not Implemented"
    | WL -> failwith "Not Implemented"
    | PV _ -> failwith "Not Implemented"

// Exercise 3.2
let rec arithEvalState a s =
    match a with
    | N a0 -> a0
    | V a0 -> Map.tryFind a0 s |> Option.defaultValue 0
    | Add (x, y) -> arithEvalState x s + arithEvalState y s
    | Sub (x, y) -> arithEvalState x s - arithEvalState y s
    | Mul (x, y) -> arithEvalState x s * arithEvalState y s
    | WL -> failwith "Not Implemented"
    | PV _ -> failwith "Not Implemented"

// Exercise 3.3
let rec arithEval a (w: word) s =
    match a with
    | N a0 -> a0
    | V a0 -> Map.tryFind a0 s |> Option.defaultValue 0
    | WL -> List.length w
    | PV a0 -> List.item (arithEval a0 w s) w |> snd
    | Add (x, y) -> arithEval x w s + arithEval y w s
    | Sub (x, y) -> arithEval x w s - arithEval y w s
    | Mul (x, y) -> arithEval x w s * arithEval y w s

// Exercise 3.4
let rec charEval c (w: word) s =
    match c with
    | C c0 -> c0
    | ToUpper c0 -> System.Char.ToUpper(charEval c0 w s)
    | ToLower c0 -> System.Char.ToLower(charEval c0 w s)
    | CV a -> List.item (arithEval a w s) w |> fst

// Exercise 3.5
let rec boolEval b w s =
    let isVowel c =
        (System.Char.ToUpper >> List.contains) c [ 'A'; 'E'; 'I'; 'O'; 'U'; 'Y' ]

    let eval f g x y = f (g x w s) (g y w s)

    match b with
    | TT -> true
    | FF -> false
    | AEq (x, y) -> eval (=) arithEval x y
    | ALt (x, y) -> eval (<) arithEval x y
    | Not b0 -> boolEval b0 w s |> not
    | Conj (b1, b2) -> eval (&&) boolEval b1 b2
    | IsDigit c -> charEval c w s |> System.Char.IsDigit
    | IsLetter c -> charEval c w s |> System.Char.IsLetter
    | IsVowel c -> charEval c w s |> isVowel
    | IfFunc (c, f) -> f (charEval c w s)


(*
    YELLOW
*)

// Exercise 3.6
let isConsonant c = ~~(IsVowel c)

// Exercise 3.7
let rec evalStmnt stm w s =
    match stm with
    | Skip -> s
    | Ass (x, a) -> Map.add x (arithEval a w s) s
    | Seq (stm1, stm2) -> evalStmnt stm1 w s |> evalStmnt stm2 w
    | ITE (guard, stm1, stm2) ->
        if boolEval guard w s then
            evalStmnt stm1 w s
        else
            evalStmnt stm2 w s
    | While (guard, stm0) ->
        if boolEval guard w s then
            evalStmnt stm0 w s
            |> evalStmnt (While(guard, stm0)) w
        else
            s

// Exercise 3.8
let stmntToSquareFun: stmnt -> squareFun =
    fun stm w pos acc ->
        let s =
            Map.ofList [ ("_pos_", pos)
                         ("_acc_", acc) ]

        evalStmnt stm w s |> Map.find "_result_"

let singleLetterScore = stmntToSquareFun (Ass("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass("_result_", arithTripleLetterScore))

let doubleWordScore = stmntToSquareFun (Ass("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass("_result_", arithTripleWordScore))


(*
    RED
*)

// Exercise 3.9
let oddConsonants: stmnt =
    let vowels = [ 'A'; 'E'; 'I'; 'O'; 'U' ]

    let isConsonant c =
        (System.Char.ToUpper >> List.contains) c vowels
        |> not

    Seq(
        Ass("_result_", V "_acc_"),
        While(
            V "i" .<. WL,
            Seq(
                ITE(IfFunc((CV(V "i")), isConsonant), Ass("_result_", V "_result_" .*. N -1), Skip),
                Ass("i", V "i" .+. N 1)
            )
        )
    )

// Exercise 3.10
let calculatePoints: square list -> word -> int =
    fun squares w ->
        let applyWord i (priority, f) = (priority, f w i)

        let calculate =
            (List.mapi (fun i sq -> List.map (applyWord i) sq))
            >> (List.fold (fun s sq -> sq @ s) [])
            >> (List.sortBy fst)
            >> (List.map snd)
            >> (List.fold (fun s f -> f s)) 0

        calculate squares

let calculatePoints2: square2 list -> word -> int =
    fun squares w ->
        let calculate =
            (List.map (List.map (fun sq2 -> (fst sq2, stmntToSquareFun (snd sq2)))))
            >> calculatePoints

        calculate squares w
