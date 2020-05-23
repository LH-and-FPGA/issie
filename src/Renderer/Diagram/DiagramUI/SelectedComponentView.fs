(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open JSHelpers
open Helpers
open DiagramModelType
open DiagramTypes
open MemoryEditorView

let private makeDescription compType model dispatch =
    match compType with
    | Input width -> div [] [ str <| sprintf "Input: %d bit(s)" width ]
    | Output width -> div [] [ str <| sprintf "Output: %d bit(s)" width ]
    | Not | And | Or | Xor | Nand | Nor | Xnor ->
        div [] [ str <| sprintf "%A gate" compType ]
    | Mux2 -> div [] [ str "Multiplexer with two inputs and one output" ]
    | Demux2 -> div [] [ str "Demultiplexer with one input and two outputs" ]
    | MergeWires -> div [] [ str "Merge two wires of width n and m into a single wire of width n+m "]
    | SplitWire width -> div [] [ str <| sprintf "Split a wire of width n+m into two wires of widthn and m (n is %d)" width]
    | Custom custom ->
        let toHTMLList =
            List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
        div [] [
            str <| sprintf "%s: user defined component" custom.Name
            br []
            span [Style [FontStyle "italic"]] [str <| "Inputs"]
            ul [] (toHTMLList custom.InputLabels)
            span [Style [FontStyle "italic"]] [str <| "Outputs"]
            ul [] (toHTMLList custom.OutputLabels)
        ]
    | DFF -> div [] [ str "D-flip-flop. The component is implicitly connected to the global clock." ]
    | ROM mem ->
        div [] [
            str "Synchronous ROM: the new data are put on the wire only upon a clock tick. The component is implicitly connected to the global clock."
            br []
            br []
            str <| sprintf "Address width: %d bit(s)" mem.AddressWidth
            br []
            str <| sprintf "Number of elements: %d" (pow2 mem.AddressWidth)
            br []
            str <| sprintf "Word width: %d bit(s)" mem.WordWidth
            br []
            br []
            Button.button [
                Button.Color IsInfo
                Button.OnClick (fun _ -> openMemoryEditor mem model dispatch)
            ] [str "View/Edit memory content"]
        ]

let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]

let private formField name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Control.div [] [ Input.text [
            Input.Props [ Name name; ]
            Input.DefaultValue defaultValue
            Input.OnChange (getTextEventValue >> onChange)
        ] ]
    ]

let viewSelectedComponent model dispatch =
    match model.SelectedComponent with
    | None -> div [] [ str "Select a component in the diagram to view/edit its properties" ]
    | Some comp ->
        div [] [
            readOnlyFormField "Description" <| makeDescription comp.Type model dispatch
            formField "Label" comp.Label (fun text -> model.Diagram.EditComponentLabel comp.Id text)
        ]
