open System
open System.IO

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

    let private getArgumentValue (variables: int[]) = function
        | Variable index -> variables[index]
        | Value value -> value

    let run (monad: Instruction[]) (input: byte[]) =
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

    module Expression =
        type [<RequireQualifiedAccess>] Expr =
            | Literal of int
            | Input of int
            | Add of Expr * Expr
            | Mul of Expr * Expr
            | Div of Expr * Expr
            | Mod of Expr * Expr
            | Eql of Expr * Expr

            override this.ToString() =
                match this with
                | Literal i -> string i
                | Input i -> $"i[{i}]"
                | Add (l, r) -> $"(%O{l}) + (%O{r})"
                | Mul (l, r) -> $"(%O{l}) * (%O{r})"
                | Div (l, r) -> $"(%O{l}) / (%O{r})"
                | Mod (l, r) -> $"(%O{l}) %% (%O{r})"
                | Eql (l, r) -> $"(%O{l}) == (%O{r})"
        
        let private getArgumentValue (variables: Expr[]) = function
            | Variable index -> variables[index]
            | Value value -> Expr.Literal value

        let runSymbolic (monad: Instruction[]) =
            let variables = Array.create 4 (Expr.Literal 0)
            let mutable inputIndex = 0
            for instr in monad do
                let result =
                    match instr with
                    | Inp _ ->
                        inputIndex <- inputIndex + 1
                        Expr.Input (inputIndex - 1)
                    | Add (var, arg) ->
                        match variables[var], getArgumentValue variables arg with
                        | Expr.Literal l, Expr.Literal r -> Expr.Literal (l + r)
                        | l, Expr.Literal 0 -> l
                        | Expr.Literal 0, r -> r
                        | l, r -> Expr.Add (l, r)
                    | Mul (var, arg) ->
                        match variables[var], getArgumentValue variables arg with
                        | Expr.Literal l, Expr.Literal r -> Expr.Literal (l * r)
                        | l, Expr.Literal 1 -> l
                        | Expr.Literal 1, r -> r
                        | _, Expr.Literal 0 | Expr.Literal 0, _ -> Expr.Literal 0
                        | l, r -> Expr.Mul (l, r)
                    | Div (var, arg) ->
                        match variables[var], getArgumentValue variables arg with
                        | Expr.Literal l, Expr.Literal r -> Expr.Literal (l / r)
                        | l, Expr.Literal 1 -> l
                        | Expr.Literal 0, _ -> Expr.Literal 0
                        | l, r -> Expr.Div (l, r)
                    | Mod (var, arg) ->
                        match variables[var], getArgumentValue variables arg with
                        | Expr.Literal l, Expr.Literal r -> Expr.Literal (l % r)
                        | Expr.Literal 0, _ -> Expr.Literal 0
                        | l, r -> Expr.Mod (l, r)
                    | Eql (var, arg) ->
                        match variables[var], getArgumentValue variables arg with
                        | Expr.Literal l, Expr.Literal r -> Expr.Literal (if l = r then 1 else 0)
                        | Expr.Literal i, Expr.Eql (_, _)
                        | Expr.Eql (_, _), Expr.Literal i when i <> 0 && i <> 1 -> Expr.Literal 0
                        | l, r when l = r -> Expr.Literal 1
                        | l, r -> Expr.Eql (l, r)
                variables[getDestinationVariable instr] <- result
            variables[3]

let monad =
    File.ReadAllLines("input.txt") |> Monad.parse

let next (valueArray: byte[]) = 
    let rec minusOne' index =
        if index < 0 then
            failwithf "Cannot find next value from %A" valueArray
        else if valueArray[index] = 1uy then
            valueArray[index] <- 9uy
            minusOne' (index - 1)
        else
            valueArray[index] <- valueArray[index] - 1uy
    minusOne' 13
    
Monad.Expression.runSymbolic monad
|> printfn "%O"