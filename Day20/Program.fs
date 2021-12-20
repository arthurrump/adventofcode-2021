open System
open System.IO

let input = File.ReadAllLines("input.txt")

let enhancementAlgorithm =
    input[0].ToCharArray() |> Array.map (fun ch -> ch = '#')

let image =
    input[2..] 
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun ch -> ch = '#'))

let printImage (image: bool[][]) =
    for line in image do
        for cell in line do
            if cell then printf "#" else printf "."
        printfn ""

let extendImage (image: bool[][]) (outerValue: bool) =
    [| Array.create (image.Length + 4) outerValue
       Array.create (image.Length + 4) outerValue
       yield! image |> Array.map (fun line -> [| outerValue; outerValue; yield! line; outerValue; outerValue |])
       Array.create (image.Length + 4) outerValue
       Array.create (image.Length + 4) outerValue |]

let toDec binArray =
    let rec toDec = function
        | [] -> 0
        | true::rest -> 1 + 2 * toDec rest
        | false::rest -> 2 * toDec rest
    toDec (List.rev binArray)

let getEnhancedPixel (image: bool[][]) (y, x) =
    let index = 
        image[y-1..y+1] 
        |> Array.collect (fun row -> row[x-1..x+1]) 
        |> Array.toList
        |> toDec
    enhancementAlgorithm[index]

let enhanceImage (image: bool[][], outerValue: bool) =
    let image = extendImage image outerValue
    let image =
        [| for y in 1..(image.Length - 2) ->
            [| for x in 1..(image[y].Length - 2) ->
                getEnhancedPixel image (y, x) |] |]
    let outerValue = enhancementAlgorithm[toDec (List.replicate 9 outerValue)]
    image, outerValue

let rec enhanceImageRepeated n (image: bool[][], outerValue: bool) =
    if n = 0
    then image
    else enhanceImageRepeated (n - 1) (enhanceImage (image, outerValue))

(image, false)
#if part1
|> enhanceImageRepeated 2
#else
|> enhanceImageRepeated 50
#endif
|> Array.collect id
|> Seq.filter id
|> Seq.length
|> printfn "%d"
