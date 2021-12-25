open System
open System.Collections.Generic
open System.IO

// With thanks to https://www.reddit.com/r/adventofcode/comments/rnl0vn/2021_day_24_had_me_for_a_sec/hpuvs50/?utm_source=reddit&utm_medium=web2x&context=3

let monadValues =
    File.ReadAllLines("input.txt")
    |> Array.splitInto 14
    |> Array.map (fun block ->
        let [| _; _; div |] = block[4].Split(" ")
        let [| _; _; chk |] = block[5].Split(" ")
        let [| _; _; add |] = block[15].Split(" ")
        (int div, int chk, int add))

let find (inputs: int[]) monadValues =
    let z = Stack()
    for (i, (div, chk, add)) in Array.indexed monadValues do
        if div = 1 then
            z.Push((i, inputs[i] + add))
        else
            let (lastI, lastValue) = z.Pop()
            if lastValue + chk <> inputs[i] then
                inputs[i] <- lastValue + chk
                if inputs[i] < 1 then
                    let diff = 1 - inputs[i]
                    inputs[i] <- inputs[i] + diff
                    inputs[lastI] <- inputs[lastI] + diff
                else if inputs[i] > 9 then
                    let diff = inputs[i] - 9
                    inputs[i] <- inputs[i] - diff
                    inputs[lastI] <- inputs[lastI] - diff
                if inputs[lastI] < 0 || inputs[lastI] > 9 then
                    failwithf "Failed at reconciling values of %i and %i" lastI i
    inputs

find (Array.create 14 9) monadValues |> printfn "%A"
find (Array.create 14 1) monadValues |> printfn "%A"
