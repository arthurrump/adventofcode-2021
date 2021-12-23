open System
open System.Collections.Generic
open System.IO

type Amphipod = char
type Location = Hallway of index: int | Room of Amphipod * index: int
type Situation = (Amphipod * Location) list

// Location indices (in hex):
// #############
// #0123456789A#
// ###0#0#0#0###
//   #1#1#1#1#
//   #########

let maxRoomIndex =
#if part1
    1
#else
    3
#endif

let startSituation: Situation =
    let diag = File.ReadAllLines("input.txt") |> Array.map (fun str -> str.ToCharArray())
    let indexToRoom i = function 3 -> Room ('A', i) | 5 -> Room ('B', i) | 7 -> Room ('C', i) | 9 -> Room ('D', i)
    [ for y in 2..3 do
        for x in 3..2..9 ->
            (diag[y][x], indexToRoom (y - 2) x) ]
    #if !part1
    |> List.map (fun (a, Room (col, i)) -> (a, Room (col, if i = 0 then 0 else 3)))
    |> List.append [ 
        ('D', Room ('A', 1)); ('C', Room ('B', 1)); ('B', Room ('C', 1)); ('A', Room ('D', 1))
        ('D', Room ('A', 2)); ('B', Room ('B', 2)); ('A', Room ('C', 2)); ('C', Room ('D', 2)) ]
    #endif

module List =
    let tryMax list =
        if list |> List.isEmpty
        then None
        else Some (list |> List.max)

    let tryMin list =
        if list |> List.isEmpty
        then None
        else Some (list |> List.min)

module Amphipod =
    let stepCost = function 'A' -> 1 | 'B' -> 10 | 'C' -> 100 | 'D' -> 1000

module Location =
    let isHallway = function Hallway _ -> true | _ -> false
    let pickHallwayLeftFrom i = function Hallway j when j < i -> Some j | _ -> None
    let pickHallwayRightFrom i = function Hallway j when j > i -> Some j | _ -> None
    let isRoom = function Room _ -> true | _ -> false
    let isRoomWith f = function Room (a, i) -> f (a, i) | _ -> false
    let isMatchingRoom amphipod = function Room (a, _) -> a = amphipod | _ -> false
    
    let isOutsideRoomIndex i = i = 2 || i = 4 || i = 6 || i = 8
    let hallwayIndexForRoomId = function 'A' -> 2 | 'B' -> 4 | 'C' -> 6 | 'D' -> 8
    let hallwayIndex = function Hallway i -> i | Room (i, _) -> hallwayIndexForRoomId i

    let rec distance l1 l2 =
        match l1, l2 with
        | Hallway i1, Hallway i2 -> 
            abs (i1 - i2)
        | Room (a1, i1), Hallway _ ->
            (i1 + 1) + distance (Hallway (hallwayIndexForRoomId a1)) l2
        | Hallway _, Room (a2, i2) ->
            distance l1 (Hallway (hallwayIndexForRoomId a2)) + (i2 + 1)
        | Room (a1, i1), Room (a2, i2) ->
            if a1 = a2 
            then abs (i1 - i2)
            else (i1 + 1) + distance (Hallway (hallwayIndexForRoomId a1)) (Hallway (hallwayIndexForRoomId a2)) + (i2 + 1)

module Situation =
    let print situation =
        let printLocation location =
            situation 
            |> List.tryFind (fun (_, loc) -> loc = location)
            |> Option.map (fst >> string)
            |> Option.defaultValue "."
            |> printf "%s"
        printf " "
        for i in 0..10 do printLocation (Hallway i)
        printfn " "
        for i in 0..maxRoomIndex do
            printf "   "; printLocation (Room ('A', i))
            printf " "; printLocation (Room ('B', i))
            printf " "; printLocation (Room ('C', i))
            printf " "; printLocation (Room ('D', i))
            printfn ""
        printfn ""

    let isLocationOccupied location situation =
        situation |> List.exists (fun (_, loc) -> loc = location)

    let isOrganized situation =
        situation |> List.forall (fun (occupant, location) -> location |> Location.isMatchingRoom occupant)

    let isOragnizedForAmphipod amphipod situation =
        situation |> List.filter (fst >> (=) amphipod) |> isOrganized

    let isOragnizedForAmphipodBelowRoomIndex amphipod roomIndex situation =
        situation 
        |> List.filter (fun (_, room) -> Location.isRoomWith (fun (a, i) -> a = amphipod && i >= roomIndex) room) 
        |> List.forall (fun (occupant, _) -> occupant = amphipod)

    let mayEnterRoom amphipod situation =
        situation 
        |> List.filter (snd >> Location.isMatchingRoom amphipod) 
        |> List.forall (fun (occupant, _) -> occupant = amphipod)

    let rec reachableLocations (amphipod, location) situation =
        if Location.isMatchingRoom amphipod location && (let (Room (a, i)) = location in isOragnizedForAmphipodBelowRoomIndex a i situation) then
            []
        else
            let hallwayIndex = Location.hallwayIndex location
            let leftHallwayBound = 
                situation 
                |> List.choose (snd >> Location.pickHallwayLeftFrom hallwayIndex) 
                |> List.tryMax 
                |> Option.map (fun i -> i + 1)
                |> Option.defaultValue 0
            let rightHallwayBound = 
                situation 
                |> List.choose (snd >> Location.pickHallwayRightFrom hallwayIndex) 
                |> List.tryMin 
                |> Option.map (fun i -> i - 1)
                |> Option.defaultValue 10
            match location with
            | Hallway _ ->
                let roomReachable = 
                    let roomIndex = Location.hallwayIndexForRoomId amphipod
                    leftHallwayBound <= roomIndex && roomIndex <= rightHallwayBound
                if roomReachable && mayEnterRoom amphipod situation then
                    [maxRoomIndex..(-1)..0] 
                    |> List.tryFind (fun i -> not (isLocationOccupied (Room (amphipod, i)) situation))
                    |> Option.map (fun i -> Room (amphipod, i))
                    |> Option.toList
                else
                    []
            | Room (a, i) -> 
                [ if [0..i-1] |> List.forall (fun j -> not (isLocationOccupied (Room (a, j)) situation)) then
                    yield! reachableLocations (amphipod, Hallway hallwayIndex) situation
                    for j in leftHallwayBound..rightHallwayBound do 
                        if not (Location.isOutsideRoomIndex j) then 
                            yield Hallway j ]

    let move (amphipod, location) destination situation =
        let situation =
            situation |> List.filter (fun (occ, loc) -> (occ, loc) <> (amphipod, location))
        (amphipod, destination) :: situation

    let step situation =
        situation
        |> List.collect (fun (amphipod, location) ->
            reachableLocations (amphipod, location) situation
            |> List.map (fun destination ->
                let cost = Amphipod.stepCost amphipod * Location.distance location destination
                let newSituation = move (amphipod, location) destination situation
                cost, newSituation))

    let minCostToOrganized situation =
        situation
        |> List.sumBy (fun (amphipod, location) -> 
            let dest = Room (amphipod, 0)
            if location = dest
            then if isOragnizedForAmphipod amphipod situation then 0 else 4 * Amphipod.stepCost amphipod
            else Amphipod.stepCost amphipod * Location.distance location dest)

let aStar (start: 'node) (step: 'node -> (int * 'node) list) (isDest: 'node -> bool) (h: 'node -> int) =
    let unvisited = PriorityQueue()
    let cost = Dictionary<'node, int>()
    let previous = Dictionary<'node, 'node>()
    let mutable current = start
    cost[current] <- 0
    unvisited.Enqueue(current, h current)
    while not (isDest current) do
        current <- unvisited.Dequeue()
        // printfn "Current cost: %d" cost[current]; Situation.print current; Console.ReadLine()
        let neighbours = step current
        for (c, nextState) in neighbours do
            let tentativeCost = cost[current] + c
            match cost.TryGetValue(nextState) with
            | (true, c) when c > tentativeCost ->
                cost[nextState] <- tentativeCost
                let f = tentativeCost + h nextState
                unvisited.Enqueue(nextState, f)
                previous[nextState] <- current
            | (false, _) ->
                cost[nextState] <- tentativeCost
                let f = tentativeCost + h nextState
                unvisited.Enqueue(nextState, f)
                previous[nextState] <- current
            | _ -> 
                ()
    cost[current], previous

// for (cost, s) in Situation.step startSituation do
//     printfn "Cost %d:" cost
//     Situation.print s

let cost, _ = aStar startSituation Situation.step Situation.isOrganized Situation.minCostToOrganized
printfn "%d" cost
    