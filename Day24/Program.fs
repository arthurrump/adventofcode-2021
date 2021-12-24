open System
open System.IO

type Range<'t> = 't * 't
module Range =
    open FsCheck
    open Microsoft.FSharp.Core.LanguagePrimitives

    let single v = (v, v)
    let add (l1, l2) (r1, r2) = (l1 + r1, l2 + r2)
    let sub (l1, l2) (r1, r2) = (l1 - r2, l2 - r1)
    let mul (l1, l2) (r1, r2) = 
        let allExtremes = [ l1 * r1; l1 * r2; l2 * r1; l2 * r2 ]
        (List.min allExtremes, List.max allExtremes)
    let div (l1, l2) (r1, r2) = 
        let allExtremes =
            [ if r1 <= GenericOne && GenericOne <= r2 then
                yield l1; yield l2
              if r1 <= -GenericOne && -GenericOne <= r2 then
                yield -l1; yield -l2
              if r1 <> GenericZero then 
                yield l1 / r1; yield l2 / r1
              if r2 <> GenericZero then
                yield l1 / r2; yield l2 / r2 ]
        (List.min allExtremes, List.max allExtremes)
    let mod' (l1, l2) (r1, r2) =
        if l2 <= GenericZero && abs l1 < r1 then (max l1 (-r2 + GenericOne), min l2 GenericZero)
        else if l2 <= GenericZero then (max l1 (-r2 + GenericOne), GenericZero)
        else if l1 < GenericZero then (max l1 (-r2 + GenericOne), min l2 (r2 - GenericOne))
        else if abs l2 < r1 then (l1, l2)
        else (GenericZero, min l2 (r2 - GenericOne))
    let eql (l1, l2) (r1, r2) = 
        if l1 = l2 && l1 = r1 && r1 = r2 then (GenericOne, GenericOne)
        else if l1 <= r2 && r1 <= l2 then (GenericZero, GenericOne) 
        else (GenericZero, GenericZero)

    let addsTo (l1, l2) (r1, r2) (res1, res2) =
        let minL, maxL = res1 - r2, res2 - r1
        let minR, maxR = res1 - l2, res2 - l1
        (max l1 minL, min l2 maxL), (max r1 minR, min r2 maxR)

    let mulsTo (l1: int64, l2: int64) (r1: int64, r2: int64) (res1: int64, res2: int64) =
        (l1, l2), (r1, r2)

    let divsTo (l1: int64, l2: int64) (r1: int64, r2: int64) (res1: int64, res2: int64) =
        (l1, l2), (r1, r2)

    module private Test =
        let (.=.) left right = left = right |@ sprintf "%A = %A" left right
        let (.<=.) left right = left <= right |@ sprintf "%A <= %A" left right

        let test filter rangeF f = fun (l1: int64, l2: int64) (r1: int64, r2: int64) -> 
            let l2, r2 = l1 + abs l2, r1 + abs r2  // -8,-7 en 8,19
            let all = [ for l in l1..l2 do for r in r1..r2 do if filter l r then yield f l r ]
            let min, max = rangeF (l1, l2) (r1, r2)
            (min .<=. List.min all) .&. (List.max all .<=. max)

        Check.Quick ("add", test (fun _ _ -> true) add (+))
        Check.Quick ("sub", test (fun _ _ -> true) sub (-))
        Check.Quick ("mul", test (fun _ _ -> true) mul (*))
        Check.Quick ("div", 
            fun (l1, l2) (r1, r2) -> 
                let r2 = if r1 = GenericZero && r2 = GenericZero then GenericOne else r2
                test (fun _ r -> r <> GenericZero) div (/) (l1, l2) (r1, r2))
        Check.Quick ("mod", 
            fun (l1, l2) (r1, r2) -> 
                let r1, r2 = abs r1 + GenericOne, abs r2
                test (fun _ r -> r <> GenericZero) mod' (%) (l1, l2) (r1, r2))
        Check.Quick ("eql", test (fun _ _ -> true) eql (fun x y -> if x = y then 1L else 0L))

        let testTo xsTo x = fun (l1, l2) (r1, r2) (e1, e2) ->
            (l1 <= l2 && r1 <= r2 && e1 <= e2) ==>
                let (inRes1, inRes2) = x (l1, l2) (r1, r2)
                (inRes1 <= e2 && e1 <= inRes2) ==>
                    let (l1', l2'), (r1', r2') = xsTo (l1, l2) (r1, r2) (e1, e2)
                    let (res1, res2) = x (l1', l2') (r1', r2')
                    (res1 .<=. max inRes1 e1) .&. (min inRes2 e2 .<=. res2)

        Check.Quick ("addsTo", testTo addsTo add)
        Check.Quick ("mulsTo", testTo mulsTo mul)
        Check.Quick ("divsTo", fun (l1, l2) (r1, r2) -> (r1 <> 0L || r2 <> 0L) ==> testTo divsTo div (l1, l2) (r1, r2))

module Monad =
    type Variable = int
    type Argument = Variable of Variable | Value of int
    type Instruction =
        | Inp of Variable
        | Add of Variable * Argument
        | Mul of Variable * Argument
        | Div of Variable * Argument
        | Mod of Variable * Argument
        | Eql of Variable * Argument

    let private getDestinationVariable = function
        | Inp v -> v
        | Add (v, _) -> v
        | Mul (v, _) -> v
        | Div (v, _) -> v
        | Mod (v, _) -> v
        | Eql (v, _) -> v

    let private variableCharToIndex = 
        function "w" -> 0 | "x" -> 1 | "y" -> 2 | "z" -> 3

    let private parseArgument (str: string) =
        if Char.IsLetter(str[0])
        then Variable (variableCharToIndex str)
        else Value (int str)

    let private parseInstruction (str: string) =
        match str.Split(' ') with
        | [| "inp"; var |] -> Inp (variableCharToIndex var)
        | [| "add"; var; arg |] -> Add (variableCharToIndex var, parseArgument arg)
        | [| "mul"; var; arg |] -> Mul (variableCharToIndex var, parseArgument arg)
        | [| "div"; var; arg |] -> Div (variableCharToIndex var, parseArgument arg)
        | [| "mod"; var; arg |] -> Mod (variableCharToIndex var, parseArgument arg)
        | [| "eql"; var; arg |] -> Eql (variableCharToIndex var, parseArgument arg)

    let parse = Array.map parseInstruction

    let private getArgumentValue f (variables: 'a[]) = function
        | Variable index -> variables[index]
        | Value value -> f value

    let run (monad: Instruction[]) (input: byte[]) =
        let getArgumentValue = getArgumentValue id
        let variables = Array.create 4 0
        let mutable inputIndex = 0
        for instr in monad do
            match instr with
            | Inp var -> 
                variables[var] <- int input[inputIndex]
                inputIndex <- inputIndex + 1
            | Add (var, arg) ->
                variables[var] <- variables[var] + getArgumentValue variables arg
            | Mul (var, arg) ->
                variables[var] <- variables[var] * getArgumentValue variables arg
            | Div (var, arg) ->
                variables[var] <- variables[var] / getArgumentValue variables arg
            | Mod (var, arg) ->
                variables[var] <- variables[var] % getArgumentValue variables arg
            | Eql (var, arg) ->
                variables[var] <- if variables[var] = getArgumentValue variables arg then 1 else 0
        variables[3] = 0

    let findVariableRangesForward (monad: Instruction[]) =
        let getArgumentValue = getArgumentValue (int64 >> Range.single)
        let variables = Array.create 4 (Range.single 0L)
        monad
        |> Array.mapFold (fun (variables: Range<int64>[]) instr ->
            let result =
                match instr with
                | Inp _ -> (1L, 9L)
                | Add (var, arg) -> Range.add variables[var] (getArgumentValue variables arg)
                | Mul (var, arg) -> Range.mul variables[var] (getArgumentValue variables arg)
                | Div (var, arg) -> Range.div variables[var] (getArgumentValue variables arg)
                | Mod (var, arg) -> Range.mod' variables[var] (getArgumentValue variables arg)
                | Eql (var, arg) -> Range.eql variables[var] (getArgumentValue variables arg)
            let variables = variables |> Array.updateAt (getDestinationVariable instr) result
            (instr, variables), variables) variables

    let findInputRanges (monad: Instruction[]) =
        let instrsWithRange, finalRange = findVariableRangesForward monad
        let expectedFinalRange = finalRange |> Array.updateAt 3 (Range.single 0L)
        Array.foldBack (fun (instr, inputRange: Range<int64>[]) (inputs, expectedRange: Range<int64>[]) ->
            let inputs =
                match instr with
                | Inp var -> expectedRange[var] :: inputs
                | _ -> inputs
            let expectedRange =
                match instr with
                | Inp _ -> expectedRange
                | Add (var, Variable arg) -> 
                    let var', arg' = Range.addsTo inputRange[var] inputRange[arg] expectedRange[var]
                    expectedRange |> Array.updateAt var var' |> Array.updateAt arg arg'
                | Add (var, Value arg) ->
                    let var' = Range.sub expectedRange[var] (Range.single (int64 arg))
                    expectedRange |> Array.updateAt var var'
                | Mul (var, Variable arg) ->
                    let var', arg' = Range.mulsTo inputRange[var] inputRange[arg] expectedRange[var]
                    expectedRange |> Array.updateAt var var' |> Array.updateAt arg arg'
                | Mul (var, Value arg) when arg <> 0 ->
                    let var' = Range.div expectedRange[var] (Range.single (int64 arg))
                    expectedRange |> Array.updateAt var var'
                | Mul (var, Value _) ->
                    expectedRange |> Array.updateAt var inputRange[var]
                | Div (var, Variable arg) ->
                    let var', arg' = Range.divsTo inputRange[var] inputRange[arg] expectedRange[var]
                    expectedRange |> Array.updateAt var var' |> Array.updateAt arg arg'
                | Div (var, Value arg) ->
                    let var' = Range.mul expectedRange[var] (Range.single (int64 arg))
                    expectedRange |> Array.updateAt var var'
                | Mod (var, _)
                | Eql (var, _) ->
                    expectedRange |> Array.updateAt var inputRange[var]
            (inputs, expectedRange)) instrsWithRange ([], expectedFinalRange)
        |> fst

let monad = File.ReadAllLines("input.txt") |> Monad.parse

Monad.findInputRanges monad
|> fun x -> printfn "%A" x; x
|> List.map (fun (x, y) -> max x y |> byte)
|> List.toArray
|> Monad.run monad
|> printfn "%A"
