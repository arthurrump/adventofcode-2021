open System
open System.IO
open System.Collections.Generic

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

    let run (monad: Instruction[]) (input: int[]) =
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
        variables[3]

    type [<RequireQualifiedAccess>] Expr =
        | Literal of int
        | Input of int
        | Add of Expr list
        | Mul of Expr * Expr
        | Div of Expr * Expr
        | Mod of Expr * Expr
        | If of Assumption * Expr * Expr

        override this.ToString() =
            match this with
            | Literal i -> string i
            | Input i -> $"i{i}"
            | Add exprs -> "(" + (exprs |> List.map string |> String.concat " + ") + ")"
            | Mul (l, r) -> $"%O{l}*%O{r}"
            | Div (l, r) -> $"%O{l}/%O{r}"
            | Mod (l, r) -> $"%O{l}%%%O{r}"
            | If (a, t, f) -> 
                let t = t.ToString().Replace("\n", "\n  ")
                let f = f.ToString().Replace("\n", "\n  ")
                $"if (%O{a}) {{\n  %s{t}\n}} else {{\n  %s{f}\n}}"
    
    and [<RequireQualifiedAccess>] Assumption =
        | Eql of Expr * Expr

        override this.ToString() =
            match this with
            | Eql (l, r) -> $"%O{l} = %O{r}"

    let rec normalizeExpr expr =
        match expr with
        | Expr.Mul (Expr.Add exprs, a) 
        | Expr.Mul (a, Expr.Add exprs) -> 
            Expr.Add (exprs |> List.map (fun e -> normalizeExpr (Expr.Mul (e, a))))
        | Expr.Div (Expr.Add exprs, a) -> 
            Expr.Add (exprs |> List.map (fun e -> normalizeExpr (Expr.Div (e, a))))
        | Expr.Mul (Expr.Div (a, b), b')
        | Expr.Mul (b', Expr.Div (a, b))
        | Expr.Div (Expr.Mul (a, b), b')
        | Expr.Div (Expr.Mul (b, a), b') when b = b' -> 
            normalizeExpr a
        | Expr.Mul (Expr.If (c, t, f), a)
        | Expr.Mul (a, Expr.If (c, t, f)) ->
            Expr.If (c, normalizeExpr (Expr.Mul (t, a)), normalizeExpr (Expr.Mul (f, a)))
        | Expr.Div (Expr.If (c, t, f), a) ->
            Expr.If (c, normalizeExpr (Expr.Div (t, a)), normalizeExpr (Expr.Div (f, a)))
        | Expr.Div (a, Expr.If (c, t, f)) ->
            Expr.If (c, normalizeExpr (Expr.Div (a, t)), normalizeExpr (Expr.Div (a, f)))
        | Expr.Mod (Expr.If (c, t, f), a) ->
            Expr.If (c, normalizeExpr (Expr.Mod (t, a)), normalizeExpr (Expr.Mod (f, a)))
        | Expr.Mod (a, Expr.If (c, t, f)) ->
            Expr.If (c, normalizeExpr (Expr.Mod (a, t)), normalizeExpr (Expr.Mod (a, f)))
        | Expr.Add exprs when exprs |> List.exists (function Expr.If _ -> true | _ -> false) ->
            let (c, t, f) = exprs |> List.pick (function Expr.If (c, t, f) -> Some (c, t, f) | _ -> None)
            let exprs = exprs |> List.filter (fun e -> e <> Expr.If (c, t, f))
            Expr.If (c, normalizeExpr (Expr.Add (t::exprs)), normalizeExpr (Expr.Add (f::exprs)))
        | Expr.Add exprs ->
            let exprs = exprs |> List.map normalizeExpr
            let adds, others = exprs |> List.partition (function Expr.Add _ -> true | _ -> false)
            let addsFlattened = adds |> List.choose (function Expr.Add es -> Some es | _ -> None) |> List.collect id
            let exprs = List.append others addsFlattened
            let literals, others = exprs |> List.partition (function Expr.Literal _ -> true | _ -> false)
            let combinedLits = literals |> List.choose (function Expr.Literal i -> Some i | _ -> None) |> List.sum
            if combinedLits = 0
            then Expr.Add others
            else Expr.Add ((Expr.Literal combinedLits)::others)
        | Expr.Mul (a, b) ->
            let a, b = normalizeExpr a, normalizeExpr b
            match a, b with
            | Expr.Literal a, Expr.Literal b -> Expr.Literal (a * b)
            | a, Expr.Literal 1 | Expr.Literal 1, a -> a
            | Expr.Literal 0, _ | _, Expr.Literal 0 -> Expr.Literal 0
            | a, b -> Expr.Mul (a, b)
        | Expr.Div (a, b) ->
            match a, b with
            | Expr.Literal l, Expr.Literal r -> Expr.Literal (l / r)
            | l, Expr.Literal 1 -> l
            | Expr.Literal 0, _ -> Expr.Literal 0
            | l, r -> Expr.Div (l, r)
        | Expr.Mod (a, b) ->
            match a, b with
            | Expr.Literal l, Expr.Literal r -> Expr.Literal (l % r)
            | Expr.Literal 0, _ -> Expr.Literal 0
            | l, r -> Expr.Mod (l, r)
        | _ -> expr

    let rec runSymbolic (variables: Expr[]) (inputIndex: int) (monad: Instruction[]) =
        let getArgumentValue = getArgumentValue Expr.Literal
        let mutable inputIndex = inputIndex
        for i in 0..(monad.Length-1) do
            let result =
                match monad[i] with
                | Inp _ ->
                    inputIndex <- inputIndex + 1
                    Expr.Input (inputIndex - 1)
                | Add (var, arg) ->
                    Expr.Add [ variables[var]; getArgumentValue variables arg ]
                | Mul (var, arg) ->
                    Expr.Mul (variables[var], getArgumentValue variables arg)
                | Div (var, arg) ->
                    Expr.Div (variables[var], getArgumentValue variables arg)
                | Mod (var, arg) ->
                    Expr.Mod (variables[var], getArgumentValue variables arg)
                | Eql (var, arg) ->
                    match variables[var], getArgumentValue variables arg with
                    | Expr.Literal l, Expr.Literal r -> 
                        Expr.Literal (if l = r then 1 else 0)
                    | Expr.Literal i, Expr.Input _
                    | Expr.Input _, Expr.Literal i when i < 0 || i > 9 ->
                        Expr.Literal 0
                    | l, r when l = r -> 
                        Expr.Literal 1
                    | l, r -> 
                        let var = getDestinationVariable monad[i]
                        Expr.If (
                            Assumption.Eql (l, r), 
                            runSymbolic (variables |> Array.updateAt var (Expr.Literal 1)) inputIndex (monad[i+1..]), 
                            runSymbolic (variables |> Array.updateAt var (Expr.Literal 0)) inputIndex (monad[i+1..]))
            variables[getDestinationVariable monad[i]] <- normalizeExpr result
        variables[3]

let monads =
    File.ReadAllLines("input.txt") 
    |> Monad.parse
    |> Array.splitInto 14

open Monad

let z0 = Expr.Add [Expr.Literal 8; Expr.Input 0]

monads[1] |> Monad.runSymbolic (Array.append (Array.create 3 (Monad.Expr.Literal 0)) [| z0 |]) 1 |> printfn "%O"
