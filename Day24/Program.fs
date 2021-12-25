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

let monads =
    File.ReadAllLines("input.txt") 
    |> Monad.parse
    |> Array.splitInto 14

let nextTries (numberIndex, is, z) = 
    if numberIndex < 14 
    then [ for i in 1..9 -> (numberIndex + 1, i::is, Monad.run monads[numberIndex] [| z |]) ]
    else []

let isCorrect (numberIndex, _, z) =
    numberIndex = 14 && z = 0

let dfs start =
    let stack = Stack()
    stack.Push(start)
    let mutable current = start
    while not (isCorrect current) do
        current <- stack.Pop()
        for next in nextTries current do
            stack.Push(next)
    current

let _, modelNumber, _ = dfs (0, [], 0)
printfn "%A" (modelNumber |> List.rev)
