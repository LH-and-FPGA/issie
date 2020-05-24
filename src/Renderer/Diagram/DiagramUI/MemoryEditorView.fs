(*
    MemoryEditorView.fs

    A simple Popup editor to view and change the content of a memory.
*)

module MemoryEditorView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Helpers
open JSHelpers
open DiagramTypes
open DiagramMessageType
open DiagramModelType
open PopupView

let private headerHeight = 100;
let private headerStyle = Style [
    Position "fixed"
    MarginTop (string (-headerHeight-20) + "px")
    PaddingTop "20px"
    BackgroundColor "white"
    Width "61%"
    Height headerHeight
    ZIndex 10
]
let private bodyStyle = Style [
    MarginTop (string headerHeight + "px")
]

let private makeEditorHeader memory =
    div [headerStyle] [
        str <| sprintf "Number of elements: %d" (pow2int64 memory.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" memory.WordWidth
    ]

let private makeEditorBody memory compId model =
    let makeRow addr content =
        tr [] [
            td [] [ str <| sprintf "%d" addr ]
            td [] [
                Input.text [
                    Input.DefaultValue <| sprintf "%d" content
                    Input.OnChange (getTextEventValue >> fun text ->
                        // TODO ensure int value has not too many bits.
                        // TODO show an error popup.
                        match strToInt text with
                        | Ok value -> model.Diagram.WriteMemoryLine compId addr value
                        | Error err -> log err
                    )
                ]
            ]
        ]
    div [bodyStyle] [
        Table.table [] [
            thead [] [
                tr [] [
                    th [] [str "Address"]
                    th [] [str "Content"]
                ]
            ]
            tbody [] (
                memory.Data |> List.mapi makeRow
            )
        ]
    ]

let private makeEditor memory compId model =
    div [] [
        makeEditorHeader memory
        makeEditorBody memory compId model
    ]

let openMemoryEditor memory compId model dispatch : unit =
    let title = "Memory editor"
    let body = makeEditor memory compId model
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> dispatch ClosePopup)
                        // TODO: reload selected component when leaving memory editor.
                    ] [ str "Done" ]
                ]
            ]
        ]
    let extraStyle = [
        Width "65%"
        Height "80%"
    ]
    closablePopup title body foot extraStyle dispatch
