open System

let p1 (cs: char list) =
    match cs with
    | '1'::cs1 -> Some ("1", cs1)
    | _ -> None

type ParserResult<'a> = ('a * char list) option
type Parser<'a> = char list -> ParserResult<'a>

let run<'a> str (parser: Parser<'a>) = parser (List.ofSeq str)

run "12345" p1;;
run "abcde" p1;;

let pchar c (cs: char list) =
    match cs with
    | c1::cs1 when c = c1 -> Some (string c1, cs1)
    | _ -> None

run "12345" (pchar '1');;
pchar '1' |> run "abcde";;

let ppred p (cs: char list) =
    match cs with
    | c1::cs1 when p c1 -> Some (string c1, cs1)
    | _ -> None

ppred Char.IsDigit |> run "12345";;
ppred Char.IsDigit |> run "abcde";;

let (??>) parser defaultValue cs =
    match parser cs with
    | Some (value1, cs1) -> Some (value1, cs1)
    | None -> Some (defaultValue, cs)

pchar '1' ??> "null" |> run "12345";;
// (??>) (pchar '1') "null"
pchar '1' ??> "null" |> run "abcde";;

let (<+>) parser1 parser2 cs =
    match parser1 cs with
    | Some (value1: string, cs1) ->
        match parser2 cs1 with
        | Some (value2, cs2) -> Some (value1 + value2, cs2)
        | None -> None
    | None -> None

ppred Char.IsLetter <+> ppred Char.IsDigit |> run "a1b2c3";;
ppred Char.IsLetter <+> ppred Char.IsDigit |> run "1a2b3c";;

let manyStr parser (cs: char list) =
    let rec iter accumulator cs1 =
        match parser cs1 with
        | Some (value, cs2) -> iter (accumulator + value) cs2
        | None -> Some (accumulator, cs1)

    iter "" cs

manyStr (ppred Char.IsDigit) (List.ofSeq "12345");;
manyStr (ppred Char.IsDigit) (List.ofSeq "abcde");;

let manyStr1 parser cs = (parser <+> manyStr parser) cs

manyStr1 (ppred Char.IsDigit) (List.ofSeq "abcde");;
manyStr1 (ppred Char.IsDigit) (List.ofSeq "1abcde");;

let pstr (str: string) cs =
    let rec compare cs1 cs2 =
        match cs1, cs2 with
        | c3::cs3, c4::cs4 when c3 = c4 -> compare cs3 cs4
        | [], cs5 -> Some (str, cs5)
        | _ -> None

    compare (List.ofSeq str) cs

pstr "abcde" |> run "abcde";;
pstr "abcde" |> run "abcde12345";;
pstr "abcde" |> run "abc12";;

let (<|>) parser1 parser2 cs =
    match parser1 cs with
    | Some (value1, cs1) -> Some (value1, cs1)
    | None ->
        match parser2 cs with
        | Some (value2, cs2) -> Some (value2, cs2)
        | None -> None

pstr "abc" <|> pstr "123" |> run "12345";;
pstr "abc" <|> pstr "123" |> run "abcde";;
pstr "abc" <|> pstr "123" |> run "67890";;

let digits1 = manyStr1 (ppred Char.IsDigit)

// let pfloat = digits1 .>>. ((pchar '.' .>>. digits1) ??> "")

// pfloat (List.ofSeq "3.1415");;
// pfloat (List.ofSeq "12345");;

let (|>>) parser mapper cs =
    match parser cs with
    | Some (value, cs1) -> Some (mapper value, cs1)
    | None -> None

let pfloat = digits1 <+> ((pchar '.' <+> digits1) ??> "") |>> float

pfloat |> run "3.1415";;
pfloat |> run "12345";;

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

// expression ::= addendum ('+' addendum | '-' addendum)*
//
// addendum ::= factor ('*' addendum | '/' addendum)
//
// factor ::= '-' term
//          | term
//
// term ::= number
//        | variable
//        | variable '(' parameters ')'

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

number |> run "3.1415";;
variable |> run "pi";;
term |> run "sqrt (2 * pi)";;
factor |> run "3.1415";;
factor |> run "-pi";;
addendum |> run "3.1415 * -pi / 2.7182";;

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

// assignment ::= identifier '=' expression

let input = pstr "input" >>. spaces1 >>. identifier |>> Input
let print = pstr "print" >>. spaces1 >>. identifier |>> Print
let assignment = identifier .>> spaces .>> pchar '=' .>> spaces .>>. expression |>> Assignment
let command = input <|> print <|> assignment

command |> run "input a";;
command |> run "print x";;
command |> run "a = b + 4 * x";;
