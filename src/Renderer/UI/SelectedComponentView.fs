(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open Helpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupView



let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]

let private textFormField isRequired name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.Placeholder (if isRequired then "Name (required)" else "Name (optional)")
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]

let private intFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private intFormFieldNoMin name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width "60px"]]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private makeMemoryInfo descr mem compId model dispatch =
    div [] [
        str descr
        br []; br []
        str <| sprintf "Address width: %d bit(s)" mem.AddressWidth
        br []
        str <| sprintf "Number of elements: %d" (1UL <<< mem.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" mem.WordWidth
        br []; br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> openMemoryEditor mem compId model dispatch)
        ] [str "View/Edit memory content"]
    ]

let private makeNumberOfBitsField model (comp:Component) text setter dispatch =
    let title, width =
        match comp.Type with
        | Input w | Output w | NbitsAdder w | NbitsXor w | Register w -> "Number of bits", w
        | SplitWire w -> "Number of bits in the top (LSB) wire", w
        | BusSelection( w, _) -> "Number of bits selected: width", w
        | BusCompare( w, _) -> "Bus width", w
        | Constant(w, _) -> "Number of bits in the wire", w
        | c -> failwithf "makeNumberOfBitsField called with invalid component: %A" c
    intFormField title "60px" width 1 (
        fun newWidth ->
            if newWidth < 1
            then
                let props = errorPropsNotification "Invalid number of bits."
                dispatch <| SetPropertiesNotification props
            else
                setter comp.Id newWidth // change the JS component
                let text' = match comp.Type with | BusSelection _ -> text | _ -> formatLabelAsBus newWidth text
                setComponentLabelFromText model comp text' // change the JS component label
                let lastUsedWidth = match comp.Type with | SplitWire _ | BusSelection _ -> model.LastUsedDialogWidth | _ ->  newWidth
                dispatch (ReloadSelectedComponent (lastUsedWidth)) // reload the new component
                dispatch ClosePropertiesNotification
    )


let private makeConstantValueField model (comp:Component) setter dispatch =
    let cVal, width =
        match comp.Type with 
        | Constant(width,cVal) -> cVal, width
        | _ -> failwithf "makeConstantValuefield called from %A" comp.Type
    if width > 32 then
        let note = errorPropsNotification "Invalid Constant width"
        dispatch <| SetPropertiesNotification note
    intFormFieldNoMin "Value of the wire:" cVal (
        fun newCVal ->
            if int64 newCVal >= (1L <<< width) || int64 newCVal < -(1L <<< (width-1))
            then
                let errMsg = sprintf "Constant value too large for number of bits: %d requires more than %d bits" newCVal width
                let note = errorPropsNotification errMsg
                dispatch <| SetPropertiesNotification note
            else
                setter comp.Id newCVal // change the JS component
                let lastUsedWidth = model.LastUsedDialogWidth
                dispatch (ReloadSelectedComponent (lastUsedWidth)) // reload the new component
                dispatch ClosePropertiesNotification
    )


let private makeLsbBitNumberField model (comp:Component) setter dispatch =
    let lsbPos, infoText =
        match comp.Type with 
        | BusSelection(width,lsb) -> uint32 lsb, "Least Significant Bit number selected: lsb"
        | BusCompare(width,cVal) -> cVal, "Compare with"
        | _ -> failwithf "makeLsbBitNumberfield called from %A" comp.Type

    match comp.Type with
    | BusCompare(width, _) -> 
        intFormField infoText "120px"  (int lsbPos) 1  (
            fun cVal ->
                if cVal < 0 || uint32 cVal > uint32 ((1 <<< width) - 1)
                then
                    let note = errorPropsNotification <| sprintf "Invalid Comparison Value for bus of width %d" width
                    dispatch <| SetPropertiesNotification note
                else
                    setter comp.Id cVal // change the JS component
                    dispatch (ReloadSelectedComponent (width)) // reload the new component
                    dispatch ClosePropertiesNotification
        )
    | BusSelection(width, _) -> 
        intFormField infoText "60px" (int lsbPos) 1 (
            fun newLsb ->
                if newLsb < 0
                then
                    let note = errorPropsNotification "Invalid LSB bit position"
                    dispatch <| SetPropertiesNotification note
                else
                    setter comp.Id newLsb // change the JS component
                    dispatch (ReloadSelectedComponent (width)) // reload the new component
                    dispatch ClosePropertiesNotification
        )
    | _ -> failwithf "What? invalid component for lsbpos in properties"



let private makeDescription (comp:Component) model dispatch =
    match comp.Type with
    | Input _ -> str "Input."
    | Constant _ -> str "Constant Wire."
    | Output _ -> str "Output."
    | BusCompare _ -> str "The output is one if the bus unsigned binary value is equal to the integer specified."
    | BusSelection _ -> div [] [
                str "Bus Selection."
                br []
                str "The output is the subrange [width+lsb-1..lsb] of the input bits. If width = 1 this selects one bit. Error if the input has less than width + lsb bits."
                br []
                br []
                str "Note that the output bit(s) are numbered from 0 even if the input range has LS bit number > 0. \
                     The input bits connected are displayed in the schematic symbol"
        ]
    | IOLabel -> div [] [
        str "Label on Wire or Bus. Labels with the same name connect wires. Each label has input on left and output on right. \
            No output connection is required from a set of labels. Since a set represents one wire of bus, exactly one input connection is required. \
            Labels can be used:"  
        br [] ;
        str "To name wires and document designs."; br []
        str "To join inputs and outputs without wires."; br []
        str "To prevent an unused output from giving an error."
        ]
    | Not | And | Or | Xor | Nand | Nor | Xnor ->
        div [] [ str <| sprintf "%A gate." comp.Type ]
    | Mux2 -> div [] [ str "Multiplexer with two inputs and one output." ]
    | Demux2 -> div [] [ str "Demultiplexer with one input and two outputs." ]
    | MergeWires -> div [] [ str "Merge two wires of width n and m into a single wire of width n+m." ]
    | SplitWire _ -> div [] [ str "Split a wire of width n+m into two wires of width n and m."]
    | NbitsAdder numberOfBits -> div [] [ str <| sprintf "%d bit(s) adder." numberOfBits ]
    | NbitsXor numberOfBits  -> div [] [ str <| sprintf "%d XOR gates with %d outputs." numberOfBits numberOfBits]
    | Decode4 -> div [] [ str <| "4 bit decoder: Data is output on the Sel output, all other outputs are 0."]
    | Custom custom ->
        let toHTMLList =
            List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
        div [] [
            str <| sprintf "%s: user defined component." custom.Name
            br []
            span [Style [FontStyle "italic"]] [str <| "Inputs"]
            ul [] (toHTMLList custom.InputLabels)
            span [Style [FontStyle "italic"]] [str <| "Outputs"]
            ul [] (toHTMLList custom.OutputLabels)
        ]
    | DFF -> div [] [ str "D-flip-flop. The component is implicitly connected to the global clock." ]
    | DFFE -> div [] [
        str "D-flip-flop with enable. If the enable signal is high the state of
             the D-flip-flop will be updated at the next clock cycle.
             The component is implicitly connected to the global clock." ]
    | Register _  -> div [] [ str "Register. The component is implicitly connected to the global clock." ]
    | RegisterE _ ->
        div [] [ str "Register with enable. If the enable signal is high the
                      state of the Register will be updated at the next clock
                      cycle. The component is implicitly connected to the global
                      clock." ]
    | AsyncROM mem ->
        let descr = "Asynchronous ROM: the output is updated as soon as the address changes."
        makeMemoryInfo descr mem comp.Id model dispatch
    | ROM mem ->
        let descr = "Synchronous ROM: the output is updated only after a clock tick. The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem comp.Id model dispatch
    | RAM mem ->
        let descr =
            "RAM memory. At every clock tick, the RAM can either read or write
            the content of the memory location selected by the address. If the
            write signal is high, the content of the selected memory location
            is set to the value of data-in. This value will also be propagated
            to data-out immediately. The component is implicitly connected to
            the global clock."
        makeMemoryInfo descr mem comp.Id model dispatch

let private makeExtraInfo model (comp:Component) text dispatch =
    match comp.Type with
    | Input _ | Output _ | NbitsAdder _ | NbitsXor _ ->
        makeNumberOfBitsField model comp text model.Diagram.SetNumberOfBits dispatch
    | SplitWire _ ->
        makeNumberOfBitsField model comp text model.Diagram.SetTopOutputWidth dispatch
    | Register _ ->
        makeNumberOfBitsField model comp text model.Diagram.SetRegisterWidth dispatch
    | BusSelection _ -> 
        div [] [
            makeNumberOfBitsField model comp text model.Diagram.SetNumberOfBits dispatch
            makeLsbBitNumberField model comp model.Diagram.SetLsbBitNumber dispatch
            ]
    | BusCompare _ -> 
        div [] [
            makeNumberOfBitsField model comp text model.Diagram.SetNumberOfBits dispatch
            makeLsbBitNumberField model comp model.Diagram.SetCompareVal dispatch
            ]

    | Constant _ ->
        div [] [
             makeNumberOfBitsField model comp text model.Diagram.SetNumberOfBits dispatch
             makeConstantValueField model comp model.Diagram.SetConstantNumber dispatch
             ]
    | _ -> div [] []

let viewSelectedComponent model dispatch =
    match model.SelectedComponent with
    | None -> div [] [ str "Select a component in the diagram to view or change its properties, for example number of bits." ]
    | Some comp ->
        div [Key comp.Id] [
            let label' = extractLabelBase comp.Label
            readOnlyFormField "Description" <| makeDescription comp model dispatch
            makeExtraInfo model comp label' dispatch
            let required = match comp.Type with | SplitWire _ | MergeWires | BusSelection _ -> false | _ -> true
            textFormField required "Component Name" label' (fun text -> 
                setComponentLabel model comp (formatLabel comp text)
                //updateNames model (fun _ _ -> model.WaveSim.Ports) |> StartWaveSim |> dispatch
                dispatch (ReloadSelectedComponent model.LastUsedDialogWidth) // reload the new component
                )
        ]
