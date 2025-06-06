(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView
open EEExtensions
open VerilogTypes
open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open NumberHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupHelpers
open UIPopups
open Notifications
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open CatalogueView
open TopMenuView
open MenuHelpers
open ParameterTypes

open CatalogueView.Constants

module Constants =
    let labelUniqueMess = "Components must have a unique label within one sheet"
    let dropDownHeightFraction = 2.5

    


let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]




let private textFormField isRequired name defaultValue isBad onChange onDeleteAtEnd =
    let onDelete (ev: Browser.Types.KeyboardEvent) =
        if ev.key = "Delete" then  
            let textEl: Browser.Types.HTMLInputElement = unbox (Browser.Dom.document.getElementById "labelInputElement")
            let length = textEl.value.Length
            let start = textEl.selectionStart
            if length = start then
                // Delete pressed at end of input box should go to draw block as
                // a single component DELETE action - since that was probably wanted.
                // NB it will only happen if just one component is highlighted
                onDeleteAtEnd()
            
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ 
                Id "labelInputElement"; 
                OnPaste preventDefault; 
                SpellCheck false; 
                Name name; 
                Style [ Width "200px"]; 
                OnKeyDown onDelete]
            Input.DefaultValue defaultValue
            Input.CustomClass "www"
            Input.Placeholder (if isRequired then "Name (required)" else "Name (optional)")
            Input.OnChange (getTextEventValue >> onChange)
        ]
        br []
        span [Style [FontStyle "Italic"; Color "Red"]; Hidden (isBad = None)] [str <| Option.defaultValue "" isBad]
    ]

let private textFormFieldSimple name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ OnPaste preventDefault; SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]


let private intFormField name (width:string) (defaultValue: bigint) (minValue: int)  onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%A" defaultValue
            Input.OnChange (getBigintEventValue >> onChange)
        ]
    ]

let private intFormField2 name bits (width: string) defaultValue1 defaultValue2 minValue1 minValue2 onChange1 onChange2 =
    Field.div [Field.Props[Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center]]] [
        Label.label [Label.Props [Style [Width "115px"; Flex "0 0 auto";]]] [str name]
        Label.label [Label.Props [Style [TextAlign TextAlignOptions.Center; Width "50px"; MarginRight "8px"]]] [str bits] 
        Input.number [
            Input.Props [Style [Width width;]; Min minValue1] 
            Input.DefaultValue <| sprintf "%d" defaultValue1
            Input.OnChange (getIntEventValue >> onChange1)
        ]
        Input.number [
            Input.Props [Style [Width width; MarginLeft "15px"]; Min minValue2] 
            Input.DefaultValue <| sprintf "%d" defaultValue2
            Input.OnChange (getIntEventValue >> onChange2)
        ]
        hr []
    ]

let private floatFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%A" defaultValue
            Input.OnChange (getFloatEventValue >> onChange)
        ]
    ]

let private gateTypeDropdown =
    ()


let getInitSource (mem: Memory1) (model:Model)=
    let a = mem.AddressWidth
    let path = match model.CurrentProj with | Some p -> p.ProjectPath | None -> ""
    match mem.Init with
    | SignedMultiplier -> 
        Ok $"Dout = (signed) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | UnsignedMultiplier ->
        Ok $"Dout = (unsigned) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | FromData ->
        Ok "Memory Viewer/Editor"
    | FromFile name | ToFile name | ToFileBadName name ->
        if FilesIO.fileExistsWithExtn ".ram" path name then 
            match initialiseMem mem path with
            | Ok _ -> Ok $"From '{name}.ram' file"
            | Error s -> Error $"From '{name}.ram' file. WARNING - this file exists but has a read error: 's'"
        else
            Error $"From '{name}.ram' file in folder '{path}'. WARNING - this file does not exist"
   

let getDialogMemorySetup (mem: Memory1) =
    mem.AddressWidth,mem.WordWidth,mem.Init, match mem.Init with | FromFile n -> Some n | _ -> None

let private makeMemoryInfo descr mem compId cType model dispatch =
    let setup = getDialogMemorySetup mem
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let mem1 = match cType with | Memory mem -> mem | _ -> failwithf "What? makememoryinfo called with non-memory"
    let reloadMemoryContent mem compId model dispatch =
        () // *** To be implemented
    let printMemorySource() =
        let isError, msgEnd =
            match getInitSource mem1 model with Error txt -> true, txt | Ok txt -> false, txt
        let msgStart =
            match cType with 
            | RAM1 _ | AsyncRAM1 _ -> "Initial "
            | ROM1 _ | AsyncROM1 _ -> ""
            | _ -> failwithf $"What - wrong component type ({cType}) here"
        let msg = sprintf $"{msgStart}Data Source: {msgEnd}"  
        if isError then 
            Fulma.Label.label [Label.Size IsSmall; Label.Modifiers [Modifier.TextColor IsDanger]] [str msg]
        else
            Fulma.Label.label [] [str msg]
    if model.PopupDialogData.MemorySetup = None then
        dispatch <| SetPopupDialogMemorySetup (Some setup)
    let projectPath = (Option.get model.CurrentProj).ProjectPath
    match setup with
    | (_,_,mem,_) ->
        div [] [
            str descr
            br []; br []
            bSpan $"Address width: {mem1.AddressWidth} bit(s)" 
            br []
            bSpan $"Number of elements: {1UL <<< mem1.AddressWidth}" 
            br []
            bSpan $"Word width: {mem1.WordWidth}bit(s)" 
            br []

            //makeSourceMenu model (Option.get model.CurrentProj) mem dispatch
            br [];
        
            div [] [
                Button.button [
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> dispatch <| Msg.ExecFuncInMessage(
                            (fun model _ -> openMemoryEditor mem1 compId model dispatch), dispatch))
                ] [str "View/Edit memory content"]
                (memPropsInfoButton dispatch)
                br []
                hr []

                Button.button [
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> 
                        FilesIO.openWriteDialogAndWriteMemory mem1 projectPath
                        |> (function
                                | None -> ()
                                | Some path ->
                                    let note = successPropertiesNotification $"Memory content written to '{path}'"
                                    dispatch <| SetPropertiesNotification note))
                ] [str "Export memory initial data to file"]

                
                br []; 
                br [];

                (printMemorySource())
                   
                
                (makePopupButton
                    "Memory Initial Data Source"
                    (MenuHelpers.makeSourceMenu 
                        model
                        (model.Sheet.UpdateMemory sheetDispatch) 
                        compId 
                        dispatch)
                    "Change Memory Data Source"
                    dispatch )
                hr []
                ]                
            ]   


    

let makeVerilogEditButton model (custom:CustomComponentType) dispatch : ReactElement = 
    
    let openCodeEditor code name = 
        let project' =
            match model.CurrentProj with
            |Some proj -> {proj with WorkingFileName = Some (custom.Name)}
            |None -> failwithf "Can't happen!"
        let model'= {model with CurrentProj = Some project'} 
        createVerilogPopup model true (Some code) (Some name) (UpdateVerilogFile name)
    
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        match custom.Form with
        |Some (Verilog name) ->
            let folderPath = project.ProjectPath
            let path = pathJoin [| folderPath; name + ".v" |]
            let code = 
                match tryReadFileSync path with
                |Ok text -> text
                |Error _ -> sprintf "Error: file {%s.v} has been deleted from the project directory" name
            div []
                [
                    br []
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> 
                            dispatch (StartUICmd SaveSheet)
                            dispatch (FileCommand (FileSaveOpenFile, dispatch))
                            dispatch <| Sheet(SheetT.DoNothing)
                            openCodeEditor code name dispatch)
                    ] [str "View/Edit Verilog code"]
                    br []
                ]
        |_ -> null

let makeVerilogDeleteButton (model:Model) (custom:CustomComponentType) dispatch : ReactElement = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        match custom.Form with
        |Some (Verilog name) ->
            let title = "Delete sheet"

            let body =
                div []
                    [ 
                        str "Are you sure you want to delete the following Verilog component?"
                        br []
                        str <| pathJoin
                                    [| project.ProjectPath;
                                    name + ".v" |]
                        br []
                        str <| "This action is irreversible." ]

            let buttonText = "Delete"

            let buttonAction =
                fun _ ->
                    dispatch (StartUICmd DeleteSheet)
                    dispatch <| ExecFuncInMessage(removeFileInProject name project,dispatch)
                    (removeFileWithExtn ".v" project.ProjectPath name)
                    dispatch ClosePopup
            div []
                [
                    br []
                    Button.button
                        [ 
                        Button.IsOutlined
                        Button.Color IsDanger
                        Button.OnClick(fun _ -> confirmationPopup title buttonText body  buttonAction dispatch) ]
                        [ str "Delete" ]
                    br []
                ]
        | _ -> null

let private changeAdderType model (comp:Component) dispatch = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
        
        let checkedCin,checkedCout =
            match comp.Type with
            |NbitsAdder w ->
                Checked true, Checked true
            |NbitsAdderNoCout w ->
                Checked true, Checked false
            |NbitsAdderNoCin w ->
                Checked false, Checked true 
            |NbitsAdderNoCinCout w ->
                Checked false, Checked false
            | _ -> failwithf "Cannot change adder type from non-adder component"

        let buttonActionCin =
            fun _ ->
                match comp.Type with
                |NbitsAdder w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCin w)
                |NbitsAdderNoCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCinCout w)
                |NbitsAdderNoCin w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdder w)
                |NbitsAdderNoCinCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCout w)
                | _ -> failwithf "Cannot change adder type from non-adder component"
        let buttonActionCout =
            fun _ ->
                match comp.Type with
                |NbitsAdder w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCout w)
                |NbitsAdderNoCin w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCinCout w)
                |NbitsAdderNoCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdder w)
                |NbitsAdderNoCinCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCin w)
                |_ -> failwithf "Cannot change adder type from non-adder component"

        div [] [
            Label.label [] [ str "Optional Ports"]
            
            Table.table [] [
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Cin"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionCin); Value "Cin"; Id "Cin-button"; Name "Cin-button"; checkedCin; Style [Height "15px"; Width "15px"]]]]
                ]
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Cout"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionCout); Value "Cout"; Id "Cout-button"; Name "Cout-button"; checkedCout; Style [Height "15px"; Width "15px"]]]]
                ]
            ]
            br []
            ]


let private changeCounterType model (comp:Component) dispatch = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
        let checkedLoad,checkedEnable=
            match comp.Type with
            |Counter _ ->
                Checked true, Checked true 
            |CounterNoEnable _ ->
                Checked true, Checked false
            |CounterNoLoad _ ->
                Checked false, Checked true 
            |CounterNoEnableLoad _ ->
                Checked false, Checked false
            | _ -> failwithf "Cannot change counter type from non-counter component"

        let buttonActionLoad =
            fun _ ->
                match comp.Type with
                |Counter w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoLoad w)
                |CounterNoEnable w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnableLoad w)
                |CounterNoLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (Counter w)
                |CounterNoEnableLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnable w)
                | _ -> failwithf "Cannot change adder type from non-adder component"
        let buttonActionEnable =
            fun _ ->
                match comp.Type with
                |Counter w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnable w)
                |CounterNoLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnableLoad w)
                |CounterNoEnable w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (Counter w)
                |CounterNoEnableLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoLoad w)
                |_ -> failwithf "Cannot change adder type from non-adder component"

        div [] [
            Label.label [] [ str "Optional Inputs"]
            Table.table [] [
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Load"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionLoad); Value "Load"; Id "Load-button"; Name "Load-button"; checkedLoad; Style [Height "15px"; Width "15px"]]]]
                ]
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Enable"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionEnable); Value "Enable"; Id "Enable-button"; Name "Enable-button"; checkedEnable; Style [Height "15px"; Width "15px"]]]]
                ]
            ]
            br []
            ]

let private makeScaleAdjustmentField model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let textw =  
        match comp.SymbolInfo with
        |Some si ->
            match si.HScale with
            |Some no -> no
            |None -> 1.0
        |None -> 1.0
    let texth =  
        match comp.SymbolInfo with
        |Some si ->
            match si.VScale with
            |Some no -> no
            |None -> 1.0
        |None -> 1.0

    div [] [
        floatFormField "Width Scale" "60px" textw 0.0 (
            fun (newWidth) ->
                if newWidth < 0.0
                then
                    let props = errorPropsNotification "Invalid number of bits."
                    dispatch <| SetPropertiesNotification props
                else
                    model.Sheet.ChangeScale sheetDispatch (ComponentId comp.Id) newWidth Horizontal
                    dispatch ClosePropertiesNotification
        )
        floatFormField "Height Scale" "60px" texth 0.0 (
            fun (newWidth) ->
                if newWidth < 0.0
                then
                    let props = errorPropsNotification "Invalid number of bits."
                    dispatch <| SetPropertiesNotification props
                else
                    model.Sheet.ChangeScale sheetDispatch (ComponentId comp.Id) newWidth Vertical
                    dispatch ClosePropertiesNotification
        )
    ]


let private makeNumberOfBitsField model (comp: Component) text dispatch =    
    let title, width, slot =
        match comp.Type with
        | Input1 (w, _) | Output w -> "Number of bits", w, IO comp.Label
        | NbitsAdder w | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w
        | NbitsXor (w, _) | NbitsAnd w | NbitsOr w |NbitsNot w 
        | Register w | RegisterE w
        | Counter w | CounterNoEnable w | CounterNoEnableLoad w |CounterNoLoad w
        | Viewer w -> "Number of bits", w, Buswidth
        | NbitSpreader w -> "Width of output bus", w, Buswidth
        | SplitWire w -> "Number of bits in the top (LSB) wire", w, Buswidth
        | BusSelection (w, _) -> "Number of bits selected: width", w, Buswidth
        | BusCompare (w, _) -> "Bus width", w, Buswidth
        | BusCompare1 (w, _, _) -> "Bus width", w, Buswidth
        | Constant1 (w, _, _) -> "Number of bits in the wire", w, Buswidth
        | c -> failwithf $"makeNumberOfBitsField called with invalid component: {c}"

    let constraints = [MinVal (PInt 1, $"{title} must be positive")]

    ParameterView.paramInputField model title 1 (Some width) constraints (Some comp) slot dispatch


let private makeNumberOfInputsField model (comp: Component) dispatch =
    let prompt, nInp =
        match comp.Type with
        | GateN (_, n) -> "Number of inputs", n
        | c -> failwithf $"makeNumberOfInputsField called with invalid component: {c}"

    let constraints = [
        MinVal (PInt 2, "Must have at least 2 inputs")
        MaxVal (PInt maxGateInputs, $"Cannot have more than {maxGateInputs} inputs")
    ]

    ParameterView.paramInputField model prompt  1 (Some nInp) constraints (Some comp) NGateInputs dispatch


let private changeMergeN model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let errText =
        model.PopupDialogData.Int
        |> Option.map (fun i ->
            if i < 2 || i > Constants.maxSplitMergeBranches then
                sprintf $"Must have between 2 and {Constants.maxSplitMergeBranches} inputs"
            else
                "")
        |> Option.defaultValue ""

    let title, nInp =
        match comp.Type with
        | MergeN n -> "Number of inputs", n
        | c -> failwithf "changeMergeN called with invalid component: %A" c

    div [] [
        span
            [Style [Color Red]]
            [str errText]

        intFormField title "60px" (bigint nInp) 2 (
            fun newInpNum ->
                let newInpNum = int newInpNum
                if newInpNum >= 2 && newInpNum <= Constants.maxSplitMergeBranches then
                    model.Sheet.ChangeMergeN sheetDispatch (ComponentId comp.Id) newInpNum
                    dispatch <| SetPopupDialogInt (Some newInpNum)
                else
                    dispatch <| SetPopupDialogInt (Some newInpNum)
        )
    ]


let private changeSplitN model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let errText =
        model.PopupDialogData.Int
        |> Option.map (fun i ->
            if i < 2 || i > Constants.maxSplitMergeBranches then
                sprintf $"Must have between 2 and {Constants.maxSplitMergeBranches} outputs"
            else
                "")
        |> Option.defaultValue ""

    let title, nInp, widths, lsbs =
        match comp.Type with
        | SplitN (nInputs, widths, lsblist) -> "Number of outputs", nInputs, widths, lsblist
        | c -> failwithf "changeSplitN called with invalid component: %A" c
    
    let changeWidths = fun (widths: int list) (newNumInputs: int ) (defaultVal : int)-> 
        match widths.Length with
        | n when n > newNumInputs -> 
            widths[..(newNumInputs-1)]
        | n when n < newNumInputs ->
            List.append widths (List.init (newNumInputs-n) (fun _ -> defaultVal)) 
        | _ -> widths

    let changeLsbs = fun (lsbs: int list) (widths: int list) (newNumInputs: int ) -> 
        match lsbs.Length with
        | n when n > newNumInputs -> 
            lsbs[..(newNumInputs-1)]
        | n when n < newNumInputs ->
            let msbs = List.map2 (fun lsb width -> lsb + width - 1) lsbs widths
            List.append lsbs (List.init (newNumInputs-n) (fun x -> x + (List.max msbs) + 1)) 
        | _ -> lsbs

    div [] [
        span
            [Style [Color Red]]
            [str errText]

        intFormField title "60px" (bigint nInp) 2 (
            fun newInpNum ->
                let newInpNum = int newInpNum
                if newInpNum >= 2 && newInpNum <= Constants.maxSplitMergeBranches then
                    let newWidths = changeWidths widths newInpNum 1
                    let newLsbs = changeLsbs lsbs widths newInpNum
                    model.Sheet.ChangeSplitN sheetDispatch (ComponentId comp.Id) newInpNum newWidths newLsbs
                    dispatch <| SetPopupDialogInt (Some newInpNum)
                else
                    dispatch <| SetPopupDialogInt (Some newInpNum)
        )
        div [Style [Display DisplayOptions.Flex; MarginLeft "180px"]] [
        // Add headers for the "Width" and "LSB" columns
            Label.label [Label.Props [Style [TextAlign TextAlignOptions.Center; MarginRight "20px"]]] [str "Width"]
            Label.label [Label.Props [Style [TextAlign TextAlignOptions.Center; MarginLeft "20px"]]] [str "LSB"]
        ]
        List.mapi2 (fun index defaultWidth defaultLsb ->
            let portTitle = 
                match defaultWidth with
                | n when n > 1 -> sprintf "Output Port %d" index
                | _ -> sprintf "Output Port %d" index
            let bits = 
                match defaultWidth with
                | n when n > 1 -> sprintf "(%d:%d)" (n+defaultLsb-1) defaultLsb
                | _ -> sprintf "(%d)" defaultLsb
            intFormField2 portTitle bits "60px" defaultWidth defaultLsb 1 0 
                (fun newWidth -> 
                    let neWidths = 
                        widths
                        |> List.mapi (fun i x -> if i = index then newWidth else x)
                    model.Sheet.ChangeSplitN sheetDispatch (ComponentId comp.Id) nInp neWidths lsbs
                    )
                (fun lsb -> 
                    let newLsbs = 
                        lsbs
                        |> List.mapi (fun i x -> if i = index then lsb else x)
                    model.Sheet.ChangeSplitN sheetDispatch (ComponentId comp.Id) nInp widths newLsbs
                    )
        ) widths lsbs
        |> div [Style [MarginBottom "20px"]] 
    ]


/// Used for Input1 Component types. Make field for users to enter a default value for
/// Input1 Components when they are undriven.
let makeDefaultValueField (model: Model) (comp: Component) dispatch: ReactElement =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let title = "Default value if input is undriven"

    let width, defValue =
        match comp.Type with
        | Input1 (w, defValue) ->
            match defValue with
            | Some defValue -> w, defValue
            | None -> w, 0I
        | _ -> failwithf "Other component types should not call this function."
    
    intFormField title "60px" defValue 0 (
        fun newValue ->
            // Check if value is within bit range
            match NumberHelpers.checkWidth width newValue with
            | Some msg ->
                let props = errorPropsNotification msg
                dispatch <| SetPropertiesNotification props
            | None ->
                model.Sheet.ChangeInputValue sheetDispatch (ComponentId comp.Id) newValue
                // reload the new component
                dispatch (ReloadSelectedComponent (model.LastUsedDialogWidth))
                dispatch <| SetPopupDialogInt (Some (int newValue))
                dispatch ClosePropertiesNotification
    )

let mockDispatchS msgFun msg =
    match msg with
    | Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol sMsg)) ->
        msgFun msg
    | _ -> ()



let msgToS = 
    BusWireT.Msg.Symbol >> SheetT.Msg.Wire >> Msg.Sheet
  
/// Return dialog fileds used by constant, or default values
let constantDialogWithDefault (w,cText) dialog =
    let w = Option.defaultValue w dialog.Int
    let cText = Option.defaultValue cText dialog.Text
    w, cText

/// Create react to change constant properties
let makeConstantDialog (model:Model) (comp: Component) (text:string) (dispatch: Msg -> Unit): ReactElement =
        let symbolDispatch msg = dispatch <| msgToS msg
        let wComp, txtComp =
            match comp.Type with | Constant1( w,_,txt) -> w,txt | _ -> failwithf "What? impossible" 
        let w = Option.defaultValue wComp model.PopupDialogData.Int
        let cText = Option.defaultValue txtComp model.PopupDialogData.Text
        let reactMsg, compTOpt = CatalogueView.parseConstant Constants.maxIssieBusWidth w cText
        match compTOpt with
        | None -> ()
        | Some (Constant1(w,cVal,cText) as compT) ->
            if compT <> comp.Type then
                model.Sheet.ChangeWidth (Sheet >> dispatch) (ComponentId comp.Id) w
                symbolDispatch <| SymbolT.ChangeConstant (ComponentId comp.Id, cVal, cText)
                dispatch (ReloadSelectedComponent w)
                dispatch ClosePropertiesNotification
        | _ -> failwithf "What? impossible"

        div [] [
                makeNumberOfBitsField model comp text dispatch
                br []
                reactMsg
                br []
                textFormFieldSimple 
                    "Enter constant value in decimal, hex, or binary:" 
                    cText 
                    (fun txt -> 
                        dispatch <| SetPopupDialogText (Some txt))
                
            ]              

/// Create react to change constant properties
let makeBusCompareDialog (model:Model) (comp: Component) (text:string) (dispatch: Msg -> Unit): ReactElement =
        let symbolDispatch msg = dispatch <| msgToS msg
        let wComp, txtComp =
            match comp.Type with | BusCompare1( w,_,txt) -> w,txt | _ -> failwithf "What? impossible1" 
        let w = Option.defaultValue wComp model.PopupDialogData.Int
        let cText = Option.defaultValue txtComp model.PopupDialogData.Text
        let reactMsg, compTOpt = CatalogueView.parseBusCompareValue Constants.maxIssieBusWidth w cText
        match compTOpt with
        | None -> ()
        | Some (BusCompare1(w,cVal,cText) as compT) ->
            if compT <> comp.Type then
                model.Sheet.ChangeWidth (Sheet >> dispatch) (ComponentId comp.Id) w
                symbolDispatch <| SymbolT.ChangeBusCompare (ComponentId comp.Id, cVal, cText)
                dispatch (ReloadSelectedComponent w)
                dispatch ClosePropertiesNotification
        | _ -> failwithf "What? impossible"

        div [] [
                makeNumberOfBitsField model comp text dispatch
                br []
                reactMsg
                br []
                textFormFieldSimple 
                    "Enter bus compare value in decimal, hex, or binary:" 
                    cText 
                    (fun txt -> 
                        dispatch <| SetPopupDialogText (Some txt))
                
            ] 

let private makeLsbBitNumberField model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let lsbPos, infoText =
        match comp.Type with 
        | BusSelection(width,lsb) -> bigint lsb, "Least Significant Bit number selected: lsb"
        | BusCompare(width,cVal) -> cVal, "Compare with"
        | BusCompare1(width,cVal,text) -> cVal, "Compare with"
        | _ -> failwithf "makeLsbBitNumberfield called from %A" comp.Type

    match comp.Type with
    | BusCompare(width, _) -> 
        intFormField infoText "120px"  lsbPos 1  (
            fun (cVal: bigint)->
                if cVal < 0I || cVal > (1I <<< width) - 1I
                then
                    let note = errorPropsNotification <| sprintf "Invalid Comparison Value for bus of width %d" width
                    dispatch <| SetPropertiesNotification note
                else
                    model.Sheet.ChangeLSB sheetDispatch (ComponentId comp.Id) cVal
                    dispatch (ReloadSelectedComponent (width)) // reload the new component
                    dispatch ClosePropertiesNotification
        )
    | BusSelection(width, _) -> 
        intFormField infoText "60px" lsbPos 1 (
            fun newLsb ->
                if newLsb < 0I
                then
                    let note = errorPropsNotification "Invalid LSB bit position"
                    dispatch <| SetPropertiesNotification note
                else
                    model.Sheet.ChangeLSB sheetDispatch (ComponentId comp.Id) (newLsb)
                    dispatch (ReloadSelectedComponent (width)) // reload the new component
                    dispatch ClosePropertiesNotification
        )
    | _ -> failwithf "What? invalid component for lsbpos in properties"



let private makeDescription (comp:Component) model dispatch =
    let gateDescription (numInputs: int) (gateType: GateComponentType) =
        let gType = $"{gateType}"
        let gTypeName = gType[0..0].ToUpper() + gType[1..gType.Length-1]
        $"{numInputs} input {gTypeName} gate."
        
    let gateNDescription (numInputs: int) (gateType: GateComponentType) =
        let gType = $"{gateType}"
        let gTypeName = gType[0..0].ToUpper() + gType[1..gType.Length-1]
        $"{gTypeName}-N block. {numInputs} {gTypeName} gates used one for each bit of the {numInputs}-bit input and output busses."
    match comp.Type with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | Input1 _ -> str "Input."
    | Constant1 _ | Constant _ -> str "Constant Wire."
    | Output _ -> str "Output."
    | Viewer _ -> str "Viewer."
    | NotConnected -> str "Not connected."
    | BusCompare _ | BusCompare1 _ -> str "The output is one if the bus unsigned binary value is equal to the integer specified. \
                                           This will display in hex on the design sheet. Busses of greater than 32 bits are not supported"
    | BusSelection _ -> div [] [
                str "Bus Selection."
                br []
                str "The output is the subrange [width+lsb-1..lsb] of the input bits. If width = 1 this selects one bit. \
                     Error if the input has less than width + lsb bits."
                br []
                br []
                str "Note that the output bit(s) are numbered from 0 even if the input range has LS bit number > 0. \
                     The input range selected for output is displayed in brackets on the symbol."
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
    | Not ->
        div [] [ str <| sprintf "%A gate." comp.Type ]
    | GateN (gateType, n) ->
        div [] [ str <| gateDescription n gateType ]
    | Mux2 -> div [] [ 
        str "Multiplexer with two inputs and one output." 
        br []
        br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(
                (fun model _ -> model.Sheet.ChangeReversedInputs (Sheet >> dispatch) (ComponentId comp.Id)),dispatch))
            ] 
            [str "Reverse Inputs"]
        ]
    | Mux4 -> div [] [ str "Multiplexer with four inputs and one output." ]
    | Mux8 -> div [] [ str "Multiplexer with eight inputs and one output." ]
    | Demux2 -> div [] [ str "Demultiplexer with one input and two outputs." ]
    | Demux4 -> div [] [ str "Demultiplexer with one input and four outputs." ]
    | Demux8 -> div [] [ str "Demultiplexer with one input and eight outputs." ]
    | MergeWires -> div [] [ str "Merge two busses of width n and m into a single bus of width n+m. \
                                  The bit numbers of the whole and each branch are shown when the component is connected." ]
    | MergeN _ -> div [] [ str "Merge N busses of various widths into a single bus. \
                                  The bit numbers of the whole and each branch are shown when the component is connected." ]
    | SplitWire _ -> div [] [ str "Split a bus of width n+m exactly into two non-overlapping busses of width n and m. \
                                   The bit numbers of the whole and each branch are shown when the component is connected."]
    | SplitN _ -> div [] [ str "Split a bus into N output busses of various widths. The output busses may overlap. \
                                The output busses need not include all of the input bits"]
    | NbitsAdder numberOfBits 
    | NbitsAdderNoCin numberOfBits 
    | NbitsAdderNoCout numberOfBits 
    | NbitsAdderNoCinCout numberOfBits 
        -> div [] [ str <| sprintf "%d bit(s) adder." numberOfBits ]
    | NbitsXor( numberOfBits, typ)  -> 
        match typ with
        | None -> gateNDescription numberOfBits Xor
        | Some Multiply -> $"{numberOfBits}X{numberOfBits}->{numberOfBits} Multiply block. This \
                              returns bits ({numberOfBits}:0) of the result. \
                              For these bits, signed and unsigned multiplication are identical"
        |> (fun text -> div [] [str <| text])
    | NbitsAnd numberOfBits  -> div [] [ str <| gateNDescription numberOfBits And]
    | NbitsOr numberOfBits  -> div [] [ str <| gateNDescription numberOfBits Or]
    | NbitsNot numberOfBits  -> div [] [ str <|
        $"Not-N block. {numberOfBits} Not gates used one for each bit of the {numberOfBits}-bit input and output busses."]
    | NbitSpreader numberOfBits  -> div [] [ str <| sprintf "Bus Spreader: every bit in the %d-bit output wire is the same as the 1-bit input. \
                                                            Used to implement sign extension and shift operations." numberOfBits]
    | Decode4 -> div [] [ str <| "4 bit decoder: Data is output on the Sel output, all other outputs are 0."]
    | Custom custom ->
        let styledSpan styles txt = span [Style styles] [str <| txt]
        let boldSpan txt = styledSpan [FontWeight "bold"] txt
        let italicSpan txt = styledSpan [FontStyle "italic"] txt

        let toHTMLList =
            List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
        
        let symbolExplanation =
            match custom.Form with
            |Some (Verilog _) -> ": Verilog Component."
            |_ -> ": user defined (custom) component."
            //TODO: remaining

        //let origLdc =
        //    match model.CurrentProj with
        //    |Some p -> p.LoadedComponents |> List.find (fun ldc -> ldc.Name = custom.Name)
        //    |None -> failwithf "What? current project cannot be None at this point in finding custom component description"
        let sheetDescription = 
            match custom.Description with
            |Some sheetDescription-> 
                div [] [
                    p [] [str "----------------"]
                    p [] [str sheetDescription]
                    p [] [str "----------------"]
                ]
            |None -> 
                br []
        let portOrderExplanation =
            match custom.Form with
            |Some (Verilog _) -> $"Input or Output ports are displayed on the '{custom.Name}' symbol sorted by the \
                    port definition order in the original Verilog file."
            |_ -> $"Input or Output ports are displayed on the '{custom.Name}' symbol sorted by the \
                    vertical position on the design sheet of the Input or Output components at the time the symbol is added."
            //TODO: remaining
        
        // let listmodel = model.CurrentProj.LoadedComponents

        div [] [
            boldSpan $"{custom.Name}"
            span [] [str <| symbolExplanation]
            sheetDescription
            makeVerilogEditButton model custom dispatch
            makeVerilogDeleteButton model custom dispatch
            br []
            p [  Style [ FontStyle "italic"; FontSize "12px"; LineHeight "1.1"]] [
                str <| portOrderExplanation]
            br []
            span [Style [FontWeight "bold"; FontSize "15px"]] [str <| "Inputs"]
            ul [] (toHTMLList custom.InputLabels)
            br []
            span [Style [FontWeight "bold"; FontSize "15px"]] [str <| "Outputs"]
            ul [] (toHTMLList custom.OutputLabels)
            br []
            makeScaleAdjustmentField model comp dispatch
            br []
            ParameterView.makeParamBindingEntryBoxes model comp custom dispatch
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
    | Counter _ |CounterNoEnable _ |CounterNoEnableLoad _ |CounterNoLoad _ ->
        div [] [ str "Counter with enable and load options. If the enable signal is high the
                      state of the counter will be updated at the next clock
                      cycle taking either the value of input d (when load is enabled)
                      or the value of out+1 (if load is disabled). 
                      The component is implicitly connected to the global clock." ]
    | AsyncROM1 mem ->
        let descr = "Asynchronous ROM: the output is updated as soon as the address changes."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | ROM1 mem ->
        let descr = "Synchronous ROM: the output is updated only after a clock tick. The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | RAM1 mem ->
        let descr =
            "synchronous read and write RAM memory. 
            At every clock tick, the RAM can read and optionally write
            the content of the memory location selected by the address. If the
            write signal is high, the content of the selected memory location
            is set to the value of data-in. In cycle 0 data-out is 0, otherwise
            data-out is the contents of the memory location addressed in the
            previous cycle, before any optional write.
            The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | AsyncRAM1 mem ->
        let descr =
            "Asynchronous read, synchronous write RAM memory. 
            At every clock tick, optionally write
            the content of the memory location selected by the address. If the
            write signal is high, the content of the selected memory location
            is set to the value of data-in. data-out is the contents of the memory 
            location addressed by the current cycle addres.
            The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | Shift _ -> 
        div [] [str "Issie Internal Error: This is an internal component and should never appear selected to view properties"]
        

let private makeExtraInfo model (comp:Component) text dispatch : ReactElement =
    match comp.Type with
    | Input1 _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                makeDefaultValueField model comp dispatch
            ]
    | GateN _ ->
        div []
            [
                makeNumberOfInputsField model comp dispatch
            ]
    | MergeN _ -> 
        div []
            [
                changeMergeN model comp dispatch
            ]
    | Output _ |NbitsAnd _ |NbitsOr _ |NbitsNot _ |NbitSpreader _ | NbitsXor _ | Viewer _ ->
        makeNumberOfBitsField model comp text dispatch
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                changeAdderType model comp dispatch
            ]
    | SplitWire _ ->
        makeNumberOfBitsField model comp text dispatch
    | SplitN _ ->
        div []
            [
                changeSplitN model comp dispatch
            ]
    | Register _ | RegisterE _ ->
        makeNumberOfBitsField model comp text dispatch
    |Counter _ |CounterNoEnable _ |CounterNoEnableLoad _ |CounterNoLoad _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                changeCounterType model comp dispatch
            ]
    | BusSelection _ -> 
        div [] [
            makeNumberOfBitsField model comp text dispatch
            makeLsbBitNumberField model comp dispatch
            ]
    | BusCompare _ -> 
        div [] [
            makeNumberOfBitsField model comp text dispatch
            makeLsbBitNumberField model comp dispatch
            ]

    |BusCompare1 _ ->
        makeBusCompareDialog model comp text dispatch
    | Constant1 _ ->         
             makeConstantDialog model comp text dispatch
    | _ -> div [] []





let viewSelectedComponent (model: ModelType.Model) dispatch =
    let dispatchWithModel (func: Model -> (Msg -> Unit) -> Unit) =
        dispatch <|ExecFuncInMessage(func, dispatch)

    let checkIfLabelIsUnique chars (symbols: SymbolT.Symbol list)  =
        match List.exists (fun (s:SymbolT.Symbol) -> s.Component.Label = chars) symbols with
        |true -> Error Constants.labelUniqueMess
        |false -> Ok chars

    let allowNoLabel =
        let symbols = model.Sheet.Wire.Symbol.Symbols
        match model.Sheet.SelectedComponents with
        | [cid] ->
            match Map.tryFind cid symbols with
            | Some {Component ={Type=MergeWires | SplitWire _ | BusSelection _ | NotConnected}} -> true
            | _ -> false
        | _ -> false

    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    /// return an OK label text, or an error message
    let formatLabelText (txt: string) compId =
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        let allowedDotPos =
            match comp.Type with
            | Custom {Name = name} -> name.Length
            | _ -> -1
        txt.ToUpper()
        |> (fun chars -> 
            let symbols = model.Sheet.Wire.Symbol.Symbols |> Map.toList |> List.filter (fun (i,s) -> i <> compId) |> List.map snd
            let badChars = 
                chars 
                |> Seq.indexed
                |> Seq.filter (fun (i,ch) -> not (Char.IsLetterOrDigitOrUnderscore ch) && (ch <> '.'  || i <> allowedDotPos))
                |> Seq.map snd
                |> Seq.map string |> String.concat ""
            match String.length chars with 
            | 0 when allowNoLabel -> 
                Ok ""
            | 0 -> 
                Error "Empty label is not allowed for this component"
            | _ when not (String.startsWithLetter chars) ->
                Error "Labels must start with a character"
            | _ when badChars.Contains "." && allowedDotPos > 0 ->
                Error $"Custom Component labels can only contain a '.' immediately after the name"
            | _ when badChars.Contains "."  ->
                Error $"Labels of normal components can only contain letters and digits and underscores, not '.'"
            | _ when badChars <> "" ->
                Error $"Labels can only contain letters and digits, not '{badChars}'"
            | _ -> 
                let currSymbol = model.Sheet.Wire.Symbol.Symbols[compId]
                match currSymbol.Component.Type with
                |IOLabel ->
                    let allSymbolsNotWireLabel = symbols |> List.filter(fun s -> s.Component.Type <> IOLabel)
                    checkIfLabelIsUnique chars allSymbolsNotWireLabel
                |_ ->
                    checkIfLabelIsUnique chars symbols           
            )
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [
            // let label' = extractLabelBase comp.Label
            // TODO: normalise labels so they only contain allowed chars all uppercase
            let defaultText = 
                match model.PopupDialogData.Text with
                | None -> comp.Label
                | Some text -> text
            let label' = formatLabelText defaultText compId // No formatting atm
            let labelText = match label' with Ok s -> s | Error e -> defaultText
            readOnlyFormField "Description" <| makeDescription comp model dispatch
            makeExtraInfo model comp labelText  dispatch
            let required = 
                match comp.Type with 
                | SplitWire _ | MergeWires | BusSelection _ | NotConnected -> false | _ -> true
            let isBad = 
                if model.PopupDialogData.BadLabel then 
                    match label' with 
                    | Ok _ -> None
                    | Error msg -> Some msg
                else    None

            //printfn $"{comp.Label}:{label'} - {isBad} - {label'}"
            textFormField 
                required 
                "Component Name" 
                defaultText 
                isBad 
                (fun text -> // onChange
                    match formatLabelText text compId with
                    | Error errorMess ->
                        dispatch <| SetPopupDialogBadLabel (true)
                        dispatch <| SetPopupDialogText (Some text)
                    | Ok label -> 
                        MenuHelpers.setComponentLabel model sheetDispatch comp label
                        dispatch <| SetPopupDialogText (Some label)
                        dispatch <| SetPopupDialogBadLabel (false)
                    dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)) // reload the new component
                ( fun () -> // onDeleteAtEndOfBox
                    let sheetDispatch sMsg = dispatch (Sheet sMsg)
                    let dispatchKey = SheetT.KeyPress >> sheetDispatch
                    dispatchKey SheetT.KeyboardMsg.DEL)
        ]    
    | _ -> 
        match model.CurrentProj with
        |Some proj ->
            let sheetName = proj.OpenFileName
            let sheetLdc = proj.LoadedComponents |> List.find (fun ldc -> ldc.Name = sheetName)
            let sheetDescription = sheetLdc.Description
            match sheetDescription with
            |None ->
                div [] [
                    p [] [str "Select a component in the diagram to view or change its properties, for example number of bits." ]    
                    br []
                    Label.label [] [str "Sheet Description"]
                    Button.button
                        [ 
                            Button.Color IsSuccess
                            Button.OnClick (fun _ ->
                                dispatchWithModel (fun model _ -> createSheetDescriptionPopup model None sheetName dispatch))
                        ]
                        [str "Add Description"]
                    br []
                    br []
                    ParameterView.viewParameters model dispatch
                    ]
            |Some descr ->
                div [] [
                    p [] [str "Select a component in the diagram to view or change its properties, for example number of bits." ]    
                    br []
                    Label.label [] [str "Sheet Description"]
                    p [] [str descr]
                    br []
                    Button.button
                        [
                            Button.Color IsInfo
                            Button.OnClick (fun _ ->
                                dispatchWithModel((fun model _ -> createSheetDescriptionPopup model sheetDescription sheetName dispatch))
                            )
                        ]
                        [str "Edit Description"]
                    br []
                    br []
                    ParameterView.viewParameters model dispatch
                    ]
        |None -> null
    |> (fun react -> div [Style [Height "calc(100vh - 150px)"; OverflowY OverflowOptions.Auto]] [react])
