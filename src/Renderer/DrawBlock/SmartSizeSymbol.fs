﻿module SmartSizeSymbol

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Optic
open Operators
open SmartHelpers

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart resize symbol" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and one symbols in the BusWire model so could use the SmartHelper 
    function for the wires.
*)

/// Record containing External Helpers required. 
/// HLP23: AUTHOR Dharmil Shah
type ExternalSmartHelpers =
    { UpdateSymbolWires: Model -> ComponentId -> Model }

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol)
    (smartHelpers: ExternalSmartHelpers) 
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    printfn $"Symbol XYPos: {symbolToSize.Pos}"
    printfn $"Component XYPos: {symbolToSize.Component.X}, {symbolToSize.Component.Y}"
    let sModel = wModel.Symbol

    let wires = [] // replace this with correct wires

    let symbol' = {symbolToSize with VScale=Some 2} // no change at the moment

    set (symbolOf_ symbolToSize.Id ) symbol' wModel

