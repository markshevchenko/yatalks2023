open System
open System.Collections.Generic
open System.IO

let pchar c (cs: char list) =
    match cs with
    | c1::cs1 when c = c1 -> Some (string c1, cs1)
    | _ -> None

let ppred p (cs: char list) =
    match cs with
    | c1::cs1 when p c1 -> Some (string c1, cs1)
    | _ -> None

let (??>) parser defaultValue (cs: char list) =
    match parser cs with
    | Some (value1, cs1) -> Some (value1, cs1)
    | None -> Some (defaultValue, cs)

let (<+>) parser1 parser2 (cs: char list) =
    match parser1 cs with
    | Some (value1: string, cs1) ->
        match parser2 cs1 with
        | Some (value2, cs2) -> Some (value1 + value2, cs2)
        | None -> None
    | None -> None

let manyStr parser (cs: char list) =
    let rec iter accumulator (cs1: char list) =
        match parser cs1 with
        | Some (value, cs2) -> iter (accumulator + value) cs2
        | None -> Some (accumulator, cs1)

    iter "" cs

let manyStr1 parser = parser <+> manyStr parser

let pstr (str: string) cs =
    let next (charEnumerator: IEnumerator<char>) =
        if charEnumerator.MoveNext()
        then Some charEnumerator.Current
        else None

    let rec iter accumulator enumerator cs1 =
        match next enumerator, cs1 with
        | Some c, c2::cs2 when c = c2 -> iter (accumulator + string c) enumerator cs2
        | None, _ -> Some (accumulator, cs1)
        | _ -> None

    iter "" (str.GetEnumerator()) cs

let (<|>) parser1 parser2 cs =
    match parser1 cs with
    | Some (value1, cs1) -> Some (value1, cs1)
    | None ->
        match parser2 cs with
        | Some (value2, cs2) -> Some (value2, cs2)
        | None -> None

let digits1 = manyStr1 (ppred Char.IsDigit)

// let pfloat = digits1 .>>. ((pchar '.' .>>. digits1) ??> "")

// pfloat (List.ofSeq "3.1415");;
// pfloat (List.ofSeq "12345");;

let (|>>) parser mapper cs =
    match parser cs with
    | Some (value, cs1) -> Some (mapper value, cs1)
    | None -> None

let pfloat = digits1 <+> ((pchar '.' <+> digits1) ??> "") |>> float

let spaces = manyStr (ppred Char.IsWhiteSpace)
let spaces1 = manyStr (ppred Char.IsWhiteSpace)

let (.>>) parser1 parser2 cs =
    match parser1 cs with
    | Some (value1, cs1) ->
        match parser2 cs1 with
        | Some (_, cs2) -> Some (value1, cs2)
        | None -> None
    | None -> None

let (>>.) parser1 parser2 cs =
    match parser1 cs with
    | Some (_, cs1) ->
        match parser2 cs1 with
        | Some (value2, cs2) -> Some (value2, cs2)
        | None -> None
    | None -> None

let letter = ppred Char.IsLetter
let digit = ppred Char.IsDigit

let identifier = letter <+> manyStr (letter <|> digit)

type Expression =
    | Constant of float
    | Variable of string
    | Addition of Expression * Expression
    | Substraction of Expression * Expression
    | Multiplication of Expression * Expression
    | Division of Expression * Expression
    | Negation of Expression
    | Sqrt of Expression

let rec number cs = (pfloat .>> spaces |>> Constant) cs
and variable cs = (identifier .>> spaces |>> Variable) cs
and term cs = (number
           <|> (pstr "sqrt" >>. spaces >>. pchar '(' >>. expression .>> pchar ')' .>> spaces |>> Sqrt)
           <|> variable
           <|> (pchar '(' >>. expression .>> pchar ')' .>> spaces)) cs
and factor cs = ((pchar '-' >>. spaces >>. term .>> spaces |>> Negation) <|> (term .>> spaces)) cs
and addendum cs =
    let rec addendumLoop left cs =
        match (pchar '*' >>. spaces >>. factor) cs with
        | Some (right, cs2) -> addendumLoop (Multiplication (left, right)) cs2
        | None ->
            match (pchar '/' >>. spaces >>. factor) cs with
            | Some (right, cs3) -> addendumLoop (Division (left, right)) cs3
            | None -> Some (left, cs)

    match factor cs with
    | Some (left, cs2) -> addendumLoop left cs2
    | None -> None

and expression cs =
    let rec expressionLoop left cs =
        match (pchar '+' >>. spaces >>. addendum) cs with
        | Some (right, cs2) -> expressionLoop (Addition (left, right)) cs2
        | None ->
            match (pchar '-' >>. spaces >>. addendum) cs with
            | Some (right, cs3) -> expressionLoop (Substraction (left, right)) cs3
            | None -> Some (left, cs)

    match addendum cs with
    | Some (left, cs2) -> expressionLoop left cs2
    | None -> None

let rec evaluate expression (env: Dictionary<string, float>) =
    match expression with
    | Constant value -> value
    | Variable name -> env.[name]
    | Addition (left, right) -> (evaluate left env) + (evaluate right env)
    | Substraction (left, right) -> (evaluate left env) - (evaluate right env)
    | Multiplication (left, right) -> (evaluate left env) * (evaluate right env)
    | Division (left, right) -> (evaluate left env) / (evaluate right env)
    | Negation x -> -(evaluate x env)
    | Sqrt x -> sqrt (evaluate x env)

let (.>>.) parser1 parser2 cs =
    match parser1 cs with
    | Some (value1, cs1) ->
        match parser2 cs1 with
        | Some (value2, cs2) -> Some ((value1, value2), cs2)
        | None -> None
    | None -> None

type Command =
    | Input of string
    | Print of string
    | Assignment of string * Expression

let input = pstr "input" >>. spaces1 >>. identifier |>> Input
let print = pstr "print" >>. spaces1 >>. identifier |>> Print
let assignment = identifier .>> spaces .>> pchar '=' .>> spaces .>>. expression |>> Assignment
let command = input <|> print <|> assignment

let args = Environment.GetCommandLineArgs() 
if args.Length <> 2 then
    Console.WriteLine("Useage: FPDemo <filename>")
    exit -1

let reader = File.OpenText args.[1]
try
    let mutable line = reader.ReadLine()
    let mutable env = Dictionary<string, float>()

    while line <> null && line <> "end" do
        match command (List.ofSeq line) with
        | Some (Input name, _) ->
            Console.Write($"{name} = ")
            let value = Console.ReadLine() |> float
            env.[name] <- value
        | Some (Print name, _) ->
            Console.WriteLine($"{name} = {env.[name]}")
        | Some (Assignment (name, expression), _) ->
            env.[name] <- evaluate expression env
        | None ->
            Console.Error.WriteLine($"Unrecognized command: {line}")

        line <- reader.ReadLine()
finally
    reader.Close()