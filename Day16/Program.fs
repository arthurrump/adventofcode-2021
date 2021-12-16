open System
open System.IO

let packet =
    File.ReadAllText("input.txt").Trim().ToCharArray()
    |> Array.collect (fun ch -> Convert.ToString(Convert.ToInt32(string ch, 16), 2).PadLeft(4, '0').ToCharArray())
    |> Array.toList

let toDec binArray =
    let rec toDec = function
        | [] -> 0I
        | '1'::rest -> 1I + 2I * toDec rest
        | '0'::rest -> 2I * toDec rest
    toDec (List.rev binArray)

type Packet =
    | Literal of version: int * value: bigint
    | Operator of version: int * typeId: int * Packet list

let parseValue (packetValue: char list) =
    let rec parseValue parsedBits = function
        | '0'::rest -> parsedBits @ rest[0..3], rest[4..]
        | '1'::rest -> parseValue (parsedBits @ rest[0..3]) rest[4..]
    let value, rest = parseValue [] packetValue
    toDec value, rest

let rec parsePacket (packet: char list) =
    // printfn "Parsing packet: %A" packet
    let version = toDec packet[0..2]
    let typeId = toDec packet[3..5]
    if typeId = 4 then // Literal
        let value, rest = parseValue packet[6..]
        let p = Literal (int version, value)
        p, rest
    else if packet[6] = '0' then // Operator, lengthId = 0
        let subLength = toDec packet[7..21]
        let subPackets = parsePackets packet[22..(22 + (int subLength) - 1)]
        Operator (int version, int typeId, subPackets), packet[22 + int subLength..]
    else // Operator, lengthId = 1
        let subCount = toDec packet[7..17]
        let subPackets, rest = parseNPackets subCount packet[18..]
        Operator (int version, int typeId, subPackets), rest

and parsePackets (packets: char list) =
    let packet, rest = parsePacket packets
    // printfn "Parsed packet (%A) from list with %A remaining" packet rest
    match rest with
    | [] -> [ packet ]
    | _ -> packet :: parsePackets rest

and parseNPackets n (packets: char list) =
    if n = 0 then 
        [], packets
    else 
        let packet, rest = parsePacket packets
        let otherPackets, rest = parseNPackets (n - 1I) rest
        packet::otherPackets, rest

let rec versionSum = function
    | Literal (version, _) -> version
    | Operator (version, _, subPackets) -> version + (subPackets |> List.sumBy versionSum)

let rec calculate = function
    | Literal (_, value) -> 
        value
    | Operator (_, 0, subs) -> // Sum
        subs |> List.sumBy calculate
    | Operator (_, 1, subs) -> // Product
        subs |> List.map calculate |> List.reduce (*)
    | Operator (_, 2, subs) -> // Minimum
        subs |> List.map calculate |> List.min
    | Operator (_, 3, subs) -> // Maximum
        subs |> List.map calculate |> List.max
    | Operator (_, 5, [ sub1; sub2 ]) -> // Greater than
        if calculate sub1 > calculate sub2 then 1I else 0I
    | Operator (_, 6, [ sub1; sub2 ]) -> // Less than
        if calculate sub1 < calculate sub2 then 1I else 0I
    | Operator (_, 7, [ sub1; sub2 ]) -> // Equal
        if calculate sub1 = calculate sub2 then 1I else 0I

parsePacket packet
|> fun res -> printfn "%A" res; res
#if part1
|> (fst >> versionSum)
#else
|> (fst >> calculate)
#endif
|> printfn "%A"
