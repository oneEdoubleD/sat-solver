module Bool

open FsCheck
open System

type Bool = True | False with
    static member (~-) (b: Bool) =
        match b with
        | True -> False
        | False -> True

let And (b1: Bool) (b2: Bool) =
    match b1, b2 with
    | True, True -> True
    | _,    _    -> False

let Or (b1: Bool) (b2: Bool) = 
    match b1, b2 with
    | _, True -> True
    | True, _ -> True
    | _,    _ -> False

let solve (p: Bool list) = 
    match p with
    | []      -> False
    | [True]  -> True
    | [False] -> False
    | _       -> List.reduce (And) p

[<EntryPoint>]
let main argv =
    let trueIsTrue (xs:Bool list) = solve xs = True
    Console.WriteLine(Check.Quick trueIsTrue)
    0 // return an integer exit code
