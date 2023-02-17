﻿module SmartWire

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)


/// Returns a list of all the bounding boxes of all the symbols in the current sheet
let getAllSymbolBoundingBoxes (model: Model) : BoundingBox list =
    let componentIDs = model.Symbol.Symbols.Keys |> List.ofSeq

    /// Takes in componentId and returns the bounding box of the corresponding symbol
    let getSymbolBoundingBox (model: Model) (componentId: ComponentId) : BoundingBox =
        let symbol = model.Symbol.Symbols[componentId]

        let symbolHeight =
            match symbol.VScale with
            | Some vScale -> symbol.Component.H * vScale
            | None -> symbol.Component.H

        let symbolWidth =
            match symbol.HScale with
            | Some hScale -> symbol.Component.W * hScale
            | None -> symbol.Component.W

        { H = symbolHeight
          W = symbolWidth
          TopLeft = symbol.Pos }

    componentIDs |> List.map (getSymbolBoundingBox model)

/// Checks if a wire intersects any symbol or not
/// Returns list of bounding boxes of symbols intersected by wire
let findWireSymbolIntersections (model: Model) (wire: Wire) : BoundingBox list =
    let allSymbolBoundingBoxes = getAllSymbolBoundingBoxes model

    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let segVertices = List.pairwise wireVertices[1 .. wireVertices.Length - 2] // do not consider the nubs

    let numBoxesIntersectedBySegment startPos endPos =
        allSymbolBoundingBoxes
        |> List.mapi (fun i boundingBox ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with
            | Some _ -> Some boundingBox // segment intersects bounding box
            | None -> None // no intersection
        )
        |> List.distinct
        |> List.filter (fun x -> x <> None)
        |> List.map (Option.get)

    segVertices
    |> List.collect (fun (startPos, endPos) -> numBoxesIntersectedBySegment startPos endPos)
    |> List.distinct

let buffer = 10.

/// Try shifting vertical seg to either - buffer or + buffer of intersected bounding boxes
/// For general case where all 3 symbols are not aligned
let tryShiftVerticalSeg (model: Model) (intersectedBoxes: BoundingBox list) (wire: Wire) : Wire option =
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let currentVerticalSegXPos = wireVertices[4].X

    let shiftVerticalSeg amountToShift =
        let prevSeg = wire.Segments[2]
        let nextSeg = wire.Segments[4]
        let newPrevSeg = { prevSeg with Length = prevSeg.Length + amountToShift }
        let newNextSeg = { nextSeg with Length = nextSeg.Length - amountToShift }

        let newSegments =
            wire.Segments[..1]
            @ [ newPrevSeg ] @ wire.Segments[3..3] @ [ newNextSeg ] @ wire.Segments[5..]

        { wire with Segments = newSegments }

    let tryShiftLeftWire =
        let leftBound = intersectedBoxes |> List.map (fun box -> box.TopLeft.X) |> List.min
        let amountToShift = currentVerticalSegXPos - leftBound + buffer
        shiftVerticalSeg -amountToShift

    let tryShiftRightWire =
        let rightBound =
            intersectedBoxes |> List.map (fun box -> box.TopLeft.X + box.W) |> List.max

        let amountToShift = rightBound - currentVerticalSegXPos + buffer
        shiftVerticalSeg amountToShift

    let leftShiftedWireIntersections =
        findWireSymbolIntersections model tryShiftLeftWire

    let rightShiftedWireIntersections =
        findWireSymbolIntersections model tryShiftRightWire

    // Check which newly generated wire has no intersections, return that
    match leftShiftedWireIntersections, rightShiftedWireIntersections with
    | [], _ -> Some tryShiftLeftWire
    | _, [] -> Some tryShiftRightWire
    | _, _ ->
        printfn "tryShiftVerticalSeg failed to find route"
        None

let getStartAndEndWirePos (wire: Wire) : XYPos * XYPos =
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let currentStartPos = wireVertices.Head
    let currentEndPos = wireVertices[wireVertices.Length - 2]

    currentStartPos, currentEndPos

type BoundingBoxAboveOrBelow =
    | Above of float
    | Below of float

/// Check if any bounding box is directly above or below startPos and endPos
/// If yes, returns a tuple of form
/// distance between pos and the furthest box above, distance between pos and the furthest box below
let isBoundingBoxAboveOrBelowPos
    (intersectedBoxes: BoundingBox list)
    (pos: XYPos)
    : BoundingBoxAboveOrBelow * BoundingBoxAboveOrBelow =

    let isBoxAboveOrBelowPos (pos: XYPos) (box: BoundingBox) : BoundingBoxAboveOrBelow option =
        if pos.X > box.TopLeft.X && pos.X < box.TopLeft.X + box.W then
            if pos.Y > box.TopLeft.Y then
                Above(pos.Y - box.TopLeft.Y) |> Some
            else
                Below(box.TopLeft.Y - pos.Y + box.H) |> Some
        else
            None

    let verticalDistances =
        intersectedBoxes
        |> List.map (isBoxAboveOrBelowPos pos)
        |> List.filter (fun x -> x <> None)
        |> List.map (Option.get)

    let largestDistanceAbove =
        if verticalDistances.Length = 0 then
            Above 0.
        else
            verticalDistances
            |> List.maxBy (fun x ->
                match x with
                | Above d -> d
                | Below _ -> 0.)

    let largestDistanceBelow =
        if verticalDistances.Length = 0 then
            Below 0.
        else
            verticalDistances
            |> List.maxBy (fun x ->
                match x with
                | Above _ -> 0.
                | Below d -> d)

    largestDistanceAbove, largestDistanceBelow


/// For case where all 3 symbols are aligned in y direction
let rec tryShiftHorizontalSeg (model: Model) (intersectedBoxes: BoundingBox list) (wire: Wire) : Wire option =
    let currentStartPos, currentEndPos = getStartAndEndWirePos wire

    let generateLongHorizontalWire firstVerticalSegLength secondVerticalSegLength =
        // Change segments index 1,3,5. Leave rest as is
        let newSegments =
            wire.Segments[..0]
            @ [ { wire.Segments[1] with Length = firstVerticalSegLength } ]
              @ wire.Segments[2..2]
                @ [ { wire.Segments[3] with Length = 0 } ]
                  @ wire.Segments[4..4]
                    @ [ { wire.Segments[5] with Length = secondVerticalSegLength } ]
                      @ wire.Segments[6..]

        { wire with Segments = newSegments }

    let tryShiftUpWire =
        let topBound = intersectedBoxes |> List.map (fun box -> box.TopLeft.Y) |> List.min
        let firstVerticalSegLength = topBound - buffer - currentStartPos.Y
        let secondVerticalSegLength = currentEndPos.Y - (topBound - buffer)
        generateLongHorizontalWire firstVerticalSegLength secondVerticalSegLength

    let tryShiftDownWire =
        let bottomBound =
            intersectedBoxes |> List.map (fun box -> box.TopLeft.Y + box.H) |> List.max

        let firstVerticalSegLength = bottomBound + buffer - currentStartPos.Y
        let secondVerticalSegLength = currentEndPos.Y - (bottomBound + buffer)
        generateLongHorizontalWire firstVerticalSegLength secondVerticalSegLength

    let upShiftedWireIntersections = findWireSymbolIntersections model tryShiftUpWire

    let downShiftedWireIntersections =
        findWireSymbolIntersections model tryShiftDownWire

    // Check which newly generated wire has no intersections, return that
    match upShiftedWireIntersections, downShiftedWireIntersections with
    | [], _ -> Some tryShiftUpWire
    | _, [] -> Some tryShiftDownWire
    | _, _ ->
        printfn "tryShiftHorizontalSeg failed to find route"

        let (Above distanceAboveFromStart, Below distanceBelowFromStart) =
            isBoundingBoxAboveOrBelowPos intersectedBoxes currentStartPos

        let (Above distanceAboveFromEnd, Below distanceBelowFromEnd) =
            isBoundingBoxAboveOrBelowPos intersectedBoxes currentEndPos

        match max distanceAboveFromStart distanceAboveFromEnd, max distanceBelowFromStart distanceBelowFromEnd with
        | distanceFromAbove, distanceFromBelow when distanceFromAbove > distanceFromBelow ->
            tryShiftHorizontalSeg model downShiftedWireIntersections tryShiftDownWire
        | distanceFromAbove, distanceFromBelow (*when distanceFromAbove <= distanceFromBelow*)  ->
            tryShiftHorizontalSeg model upShiftedWireIntersections tryShiftUpWire

// None


/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire) : Wire =

    let initialWire = autoroute model wire

    let intersectedBoxes = findWireSymbolIntersections model initialWire

    match intersectedBoxes.Length, wire.InitialOrientation with
    | 0, _ -> initialWire
    | _, initialOrientation when initialOrientation = Vertical -> initialWire
    | _ ->
        tryShiftVerticalSeg model intersectedBoxes initialWire
        |> Option.orElse (tryShiftHorizontalSeg model intersectedBoxes initialWire)
        |> Option.defaultValue initialWire
