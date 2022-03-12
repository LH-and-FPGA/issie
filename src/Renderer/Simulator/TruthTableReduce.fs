﻿module TruthTableReduce
open CommonTypes
open TimeHelpers
open SimulatorTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers
open TruthTableCreate

let rowEquals (row1: TruthTableRow) (row2: TruthTableRow) =
    if row1.Length <> row2.Length then
        false
    else
        (row1,row2)
        ||> List.forall2 (fun c1 c2 -> 
            match c1.Data, c2.Data with
            | Bits a, Bits b -> a = b
            | DC, _ -> true
            | _, DC -> true
            | _, _ -> 
                failwithf "what? Rows containing algebraic cells passed to rowEquals")
            

let tableTryFind (row: TruthTableRow) (tMap: Map<TruthTableRow,TruthTableRow>) =
    match (Map.tryFind row tMap), (rowContainsDC row) with
    | Some rhs, _ -> Some [rhs]
    | None, false -> None
    | None, true ->
        ([], Map.toList tMap)
        ||> List.fold (fun acc (lhs,rhs) -> 
            if rowEquals row lhs then
                rhs::acc
            else 
                acc)
        |> function 
            | [] -> None
            | lst -> Some lst

let isValidDCRow row table =
    match tableTryFind row table.FilteredMap with
    | None -> None
    | Some outputs -> 
        outputs
        |> List.forall (fun r -> r = outputs.Head)
        |> function
            | false -> None
            | true -> Some <| outputs.Head

let inputDCRows (input: CellIO) (inputConstraints: ConstraintSet) (table: TruthTable) bitLimit 
    : (TruthTableRow * TruthTableRow) list=
    let allInputs = table.Inputs
    let rowLimit = int(2.0**bitLimit)
    if allInputs.Length = 1 then 
        []
    else
        let inputIdx =
            match List.tryFindIndex (fun c -> c = input) allInputs with
            | None -> 
                failwithf "what? Trying to DC Reduce a table over an input not present in the table"
            | Some idx -> idx
        let otherInputs =
            ([input], allInputs)
            ||> List.except
            |> List.map (function 
            | SimIO s -> s
            | Viewer _ -> failwithf "what? TT Inputs contain a viewer")

        let constraints = inputConstraints.withoutIO input

        (otherInputs, constraints)
        ||> inputsWithCRC
        |> inputsWithARC rowLimit
        |> fst
        |> inputCombinations
        |> List.collect (fun row -> 
            let possible =
                row
                |> List.insertAt inputIdx {IO = input; Data = DC}
            match isValidDCRow possible table with
            | Some output -> [(possible,output)]
            | None -> [])
    
let reduceWithDCRow regularRows (dcLeft,dcRight) =
    regularRows
    |> List.filter (fun (regLeft,regRight) -> 
        rowEquals (dcLeft @ dcRight) (regLeft @ regRight)
        |> not)

let reduceTruthTable (inputConstraints: ConstraintSet) (table: TruthTable) bitLimit =
    let allDCRows =
        table.Inputs
        |> List.collect (fun input -> 
            inputDCRows input inputConstraints table bitLimit)
    
    let remainingRegularRows =
        (Map.toList table.FilteredMap, allDCRows)
        ||> List.fold reduceWithDCRow
    
    allDCRows @ remainingRegularRows
    |> Map.ofList
    



            