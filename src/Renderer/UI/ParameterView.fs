module ParameterView

open ParameterTypes
open EEExtensions
open VerilogTypes
open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open ModelType
open CommonTypes
open PopupHelpers
open Sheet.SheetInterface
open DrawModelType
open Optics
open Optics.Operators
open Optic
open System.Text.RegularExpressions
open Fulma.Extensions.Wikiki

//------------------------------------------------------------------------------------------------
//------------------------------ Handle parameters defined on design sheets ----------------------
//------------------------------------------------------------------------------------------------

(*
 * Parameters are symbols defined constant values that can be used in the design.
 * Parameter definitions have integer default values given in the sheet definition (properties pane).
 * These can be over-ridden per instance by definitions in the component instance (properties pane).
 * Parameter values can in general be defined using parameter expressions containing in-scope parameters
 * Parameter scope is currently defined to be all component instances on the parameter sheet.
 * Parameters are used in parameter expressions in the properties pane of components.
 *
 * See Common/parameterTypes.fs for the types used to represent parameters and parameter expressions.
 *)



// Lenses & Prisms for accessing sheet parameter information

// Accessing param info from loaded component
let defaultBindingsOfLC_ = lcParameterSlots_ >?> defaultBindings_
let paramSlotsOfLC_ = lcParameterSlots_ >?> paramSlots_

// Accessing param info of currently open sheet from model
let lcParameterInfoOfModel_ = openLoadedComponentOfModel_ >?> lcParameterSlots_ 
let paramSlotsOfModel_ = lcParameterInfoOfModel_ >?> paramSlots_
let defaultBindingsOfModel_ = lcParameterInfoOfModel_ >?> defaultBindings_

let modelToSymbols = sheet_ >-> SheetT.wire_ >-> BusWireT.symbol_ >-> SymbolT.symbols_

let symbolsToSymbol_ (componentId: ComponentId): Optics.Lens<Map<ComponentId, SymbolT.Symbol>, SymbolT.Symbol> =
    Lens.create
        (fun symbols -> 
            match Map.tryFind componentId symbols with
            | Some symbol -> symbol
            | None -> failwithf "Component %A not found in this sheet" componentId)
        (fun symbol symbols -> 
            symbols |> Map.add componentId symbol)


let symbolToComponent_ : Optics.Lens<SymbolT.Symbol, Component> =
    Lens.create
        (fun symbol -> symbol.Component)
        (fun newComponent symbol -> { symbol with Component = newComponent })


let compSlot_ (compSlotName:CompSlotName) : Optics.Lens<Component, int> = 
    Lens.create
        (fun comp ->
            match compSlotName with
            | Buswidth -> 
                match comp.Type with
                | Viewer busWidth -> busWidth
                | BusCompare1 (busWidth, _, _) -> busWidth
                | BusSelection (outputWidth, _) -> outputWidth
                | Constant1 (width, _, _) -> width
                | NbitsAdder busWidth -> busWidth
                | NbitsAdderNoCin busWidth -> busWidth
                | NbitsAdderNoCout busWidth -> busWidth
                | NbitsAdderNoCinCout busWidth -> busWidth
                | NbitsXor (busWidth, _) -> busWidth
                | NbitsAnd busWidth -> busWidth
                | NbitsNot busWidth -> busWidth
                | NbitsOr busWidth -> busWidth
                | NbitSpreader busWidth -> busWidth
                | SplitWire busWidth -> busWidth
                | Register busWidth -> busWidth
                | RegisterE busWidth -> busWidth
                | Counter busWidth -> busWidth
                | CounterNoLoad busWidth -> busWidth
                | CounterNoEnable busWidth -> busWidth
                | CounterNoEnableLoad busWidth -> busWidth
                | Shift (busWidth, _, _) -> busWidth
                | BusCompare (busWidth, _) -> busWidth
                | Input busWidth -> busWidth
                | Constant (width, _) -> width
                | _ -> failwithf $"Invalid component {comp.Type} for buswidth"
            | NGateInputs ->
                match comp.Type with
                | GateN (_, n) -> n
                | MergeN n -> n
                | SplitN (n, _, _) -> n
                | _ -> failwithf $"Invalid component {comp.Type} for gate inputs"
            | IO _ ->
                match comp.Type with
                | Input1 (busWidth, _) -> busWidth
                | Output busWidth -> busWidth
                | _ -> failwithf $"Invalid component {comp.Type} for IO"
            | CustomCompParam _ ->
                // Custom component parameters don't have direct component values
                // They are resolved through parameter bindings
                0
            | SheetParam _ ->
                failwithf $"Sheet parameters cannot be read from components"
            | ParameterTypes.LsbBitNumber ->
                match comp.Type with
                | BusSelection (_, lsb) -> lsb
                | _ -> failwithf $"Invalid component {comp.Type} for LSB bit number"
            | ParameterTypes.DefaultValue ->
                match comp.Type with
                | Input1 (_, Some defValue) -> int defValue
                | Input1 (_, None) -> 0
                | _ -> failwithf $"Invalid component {comp.Type} for default value"
        )
        (fun value comp->
                let newType = 
                    match compSlotName with
                    | Buswidth ->
                        match comp.Type with
                        | Viewer _ -> Viewer value
                        | BusCompare1 (_, compareValue, dialogText) -> BusCompare1 (value, compareValue, dialogText)
                        | BusSelection (_, outputLSBit) -> BusSelection (value, outputLSBit)
                        | Constant1 (_, constValue, dialogText) -> Constant1 (value, constValue, dialogText)
                        | NbitsAdder _ -> NbitsAdder value
                        | NbitsAdderNoCin _ -> NbitsAdderNoCin value
                        | NbitsAdderNoCout _ -> NbitsAdderNoCout value
                        | NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout value
                        | NbitsXor (_, arithmeticOp) -> NbitsXor (value, arithmeticOp)
                        | NbitsAnd _ -> NbitsAnd value
                        | NbitsNot _ -> NbitsNot value
                        | NbitsOr _ -> NbitsOr value
                        | NbitSpreader _ -> NbitSpreader value
                        | SplitWire _ -> SplitWire value
                        | Register _ -> Register value
                        | RegisterE _ -> RegisterE value
                        | Counter _ -> Counter value
                        | CounterNoLoad _ -> CounterNoLoad value
                        | CounterNoEnable _ -> CounterNoEnable value
                        | CounterNoEnableLoad _ -> CounterNoEnableLoad value
                        | Shift (_, shifterWidth, shiftType) -> Shift (value, shifterWidth, shiftType)
                        | BusCompare (_, compareValue) -> BusCompare (value, compareValue)
                        | Input _ -> Input value
                        | Constant (_, constValue) -> Constant (value, constValue)
                        | _ -> failwithf $"Invalid component {comp.Type} for buswidth"
                    | NGateInputs ->
                        match comp.Type with
                        | GateN (gateType, _) -> GateN (gateType, value)
                        | MergeN _ -> MergeN value
                        | SplitN (_, widths, lsbs) -> SplitN (value, widths, lsbs)
                        | _ -> failwithf $"Invalid component {comp.Type} for gate inputs"
                    | IO _ ->
                        match comp.Type with
                        | Input1 (_, defaultValue) -> Input1 (value, defaultValue)
                        | Output _ -> Output value
                        | _ -> failwithf $"Invalid component {comp.Type} for IO"
                    | CustomCompParam _ ->
                        // Custom component parameters don't modify component types directly
                        comp.Type
                    | SheetParam _ ->
                        failwithf $"Sheet parameters cannot be set on components"
                    | ParameterTypes.LsbBitNumber ->
                        match comp.Type with
                        | BusSelection (width, _) -> BusSelection (width, value)
                        | _ -> failwithf $"Invalid component {comp.Type} for LSB bit number"
                    | ParameterTypes.DefaultValue ->
                        match comp.Type with
                        | Input1 (width, _) -> Input1 (width, Some (bigint value))
                        | _ -> failwithf $"Invalid component {comp.Type} for default value"
                { comp with Type = newType}
)


/// Return a Lens that can be used to read or update the value of a component slot integer in the component.
/// The value is contained in the ComponentType part of a Component record.
/// The Component record will be found in various places, depending on the context.
/// For Properties changes, the Component record will be in the Model under SelectedComponent.
/// For changes in a newly created component the component is created by CatalogueView.createComponent.
/// A partial implementation of this function would be OK for MVP.
/// NB - the Lens cannot be part of the slot record because the Lens type can change depending on 'PINT.
/// Maybe this will be fixed by using a D.U. for the slot type: however for MVP
/// we can simplify things by dealing only with int parameters.
let modelToSlot_ (slot: ParamSlot) : Optics.Lens<Model, int> =
    modelToSymbols
    >-> symbolsToSymbol_ (ComponentId slot.CompId)
    >-> symbolToComponent_
    >-> compSlot_ slot.CompSlot


// evaluateParamExpression, renderParamExpression, parseExpression, and exprContainsParams
// have been moved to ParameterTypes module 


/// Evaluates a list of constraints got from slots against a set of parameter bindings to
/// check what values of param are allowed.
/// NB here 'PINT is not a polymorphic type but a type parameter that will be instantiated to int or bigint.
let evaluateConstraints
        (paramBindings: ParamBindings)
        (exprSpecs: ConstrainedExpr list)
        (dispatch: Msg -> unit)
            : Result<Unit, ParamConstraint list> =


    let failedConstraints konst expr =
        let resultExpression = ParameterTypes.evaluateParamExpression paramBindings expr
        match resultExpression with
            | Ok value ->        
                konst
                |> List.filter (fun constr ->
                    match constr with
                    | MaxVal (expr, errorMsg) -> 
                        match ParameterTypes.evaluateParamExpression paramBindings expr with
                        | Ok maxValue -> value > maxValue
                        | Error err -> // evaluation of constraint failed
                            let errMsg = sprintf "Expression Evaluation of Constraint failed because %s" (string err)
                            dispatch <| SetPopupDialogText (Some (string errMsg))
                            false
                    | MinVal (expr, _) -> 
                        match ParameterTypes.evaluateParamExpression paramBindings expr with
                        | Ok minValue -> value < minValue
                        | Error err -> // evaluation of constraint failed
                            let errMsg = sprintf "Expression Evaluation of Constraint failed because %s" (string err)
                            dispatch <| SetPopupDialogText (Some (string errMsg))
                            false
                    )
            | Error err ->
                let errMsg = sprintf "Expression Evaluation of Constraint failed because %s" (string err)
                dispatch <| SetPopupDialogText (Some (string errMsg))
                List.empty
    
    let result =
        exprSpecs
        |> List.collect (fun slot ->
            failedConstraints slot.Constraints slot.Expression)
    
    if List.isEmpty result then Ok()
    else Error result


/// Generates a ParameterExpression from input text
/// Operators are left-associative
// parseExpression has been moved to ParameterTypes module


/// Get LoadedComponent for currently open sheet
/// This cannot fail, because LoadedComponent must be loaded for sheet to be open
let getCurrentSheet model = 
    let sheetName = 
        match model.CurrentProj with
        | Some proj -> proj.OpenFileName
        | None -> failwithf "Cannot find sheet because no project is open"

    model
    |> ModelHelpers.tryGetLoadedComponents
    |> List.tryFind (fun lc -> lc.Name = sheetName)
    |> function
       | Some lc -> lc
       | None -> failwithf "No loaded component with same name as open sheet"


/// Get default parameter bindings for LoadedComponent 
let getDefaultParams loadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some paramSlots -> paramSlots.DefaultBindings
    | None -> Map.empty


/// Get default parameter slots for LoadedComponent 
let getParamSlots loadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some sheetinfo -> sheetinfo.ParamSlots
    | None -> Map.empty


/// Get current loaded component parameter info
/// Returns empty maps for ParamSlots and DefaultBindings if None
let getLCParamInfo (model: Model) =
    model
    |> get lcParameterInfoOfModel_
    |> Option.defaultValue {ParamSlots = Map.empty; DefaultBindings = Map.empty}


/// Get a loaded component from its name
/// Returns the currently open loaded component if no name is specified
let getLoadedComponent (sheetName: string option) (model: Model): LoadedComponent =
    let project = Option.get model.CurrentProj
    let lcName = sheetName |> Option.defaultValue project.OpenFileName
    project.LoadedComponents |> List.find (fun lc -> lc.Name = lcName)


/// Get a component from its ID and the name of the sheet that it is on
let getComponentById (model: Model) (sheetName: string) (compId: string): Component = 
    let project = Option.get model.CurrentProj
    
    printfn $"DEBUG: getComponentById called with sheetName='{sheetName}', compId='{compId}'"
    printfn $"DEBUG: project.OpenFileName='{project.OpenFileName}'"
    
    // Need to use current sheet symbols if sheet is open, otherwise loaded component
    if sheetName = project.OpenFileName
    then 
        try
            let comp = model.Sheet.GetComponentById <| ComponentId compId
            printfn $"DEBUG: Found component on current sheet: {comp.Label}"
            comp
        with
        | ex ->
            printfn $"DEBUG: Error getting component from current sheet: {ex.Message}"
            reraise()
    else
        let lc = getLoadedComponent (Some sheetName) model
        let (comps, _) = lc.CanvasState
        printfn $"DEBUG: Looking in loaded component '{lc.Name}' with {comps.Length} components"
        comps |> List.iter (fun c -> printfn $"DEBUG: Available component: {c.Id} ({c.Label})")
        
        match comps |> List.tryFind (fun comp -> comp.Id = compId) with
        | Some comp -> 
            printfn $"DEBUG: Found component in loaded component: {comp.Label}"
            comp
        | None ->
            printfn $"DEBUG: Component {compId} not found in sheet {sheetName}"
            failwithf $"Component {compId} not found in sheet {sheetName}"


/// Check that an expression passes its constraints for a given set of bindings
let checkConstraints
    (paramBindings: ParamBindings)
    (exprSpec: ConstrainedExpr)
    : Result<Unit, ParamError> =

    let checkConstraint constr =
        let comparator, constrExpr, errMsg = 
            match constr with
            | MaxVal (expr, err) -> (<=), expr, err
            | MinVal (expr, err) -> (>=), expr, err

        let evalExpr = ParameterTypes.evaluateParamExpression paramBindings exprSpec.Expression
        let evalConstr = ParameterTypes.evaluateParamExpression paramBindings constrExpr

        match evalExpr, evalConstr with
        | Ok value, Ok limit when comparator value limit -> Ok ()
        | Error valueErr, _ -> Error valueErr
        | _ -> Error errMsg

    exprSpec.Constraints
    |> List.map checkConstraint
    |> List.filter Result.isError
    |> function
       | [] -> Ok ()
       | firstError :: _ -> firstError


/// Checks whether all param slots on a sheet their constraints
/// Edited bindings override default bindings on that sheet
/// Returns the first broken constraint
let rec checkAllCompSlots
    (model: Model)
    (sheetName: string)
    (editedBindings: ParamBindings)
    : Result<Unit, ParamError> =

    printfn $"DEBUG: checkAllCompSlots called for sheetName='{sheetName}'"
    printfn $"DEBUG: editedBindings has {editedBindings.Count} entries"

    // Get bindings of sheet that component is on
    let toplevelLC = getLoadedComponent (Some sheetName) model
    let toplevelBindings = toplevelLC |> get defaultBindingsOfLC_ |> Option.defaultValue Map.empty
    let updatedBindings = Helpers.mapUnion editedBindings toplevelBindings

    // Add detailed component information to constraint error messages
    let addDetailedErrorMsg (slot: ParamSlot) (spec: ConstrainedExpr): ConstrainedExpr =
        printfn $"DEBUG: addDetailedErrorMsg called for slot CompId='{slot.CompId}', CompSlot='{slot.CompSlot}'"
        let renderedExpr = ParameterTypes.renderParamExpression spec.Expression 0
        let slotText =
            match slot.CompSlot with
            | Buswidth | IO _ -> $"bus width of {renderedExpr}"
            | NGateInputs -> $"{renderedExpr} inputs"
            | CustomCompParam param -> $"parameter binding of {param} = {renderedExpr}"
            | SheetParam _ -> failwithf $"Sheet parameter cannot be slot in a component"
            | ParameterTypes.LsbBitNumber -> $"LSB bit number of {renderedExpr}"
            | ParameterTypes.DefaultValue -> $"default value of {renderedExpr}"

        try
            let comp = getComponentById model sheetName slot.CompId
            let compInfo = $"{comp.Label} has {slotText}"
            printfn $"DEBUG: Successfully created compInfo: {compInfo}"

            let addDetail constr =
                match constr with
                | MaxVal (expr, err) -> MaxVal (expr, $"{compInfo}. {err}.")
                | MinVal (expr, err) -> MinVal (expr, $"{compInfo}. {err}.")

            let detailedConstraints = spec.Constraints |> List.map addDetail
            {spec with Constraints = detailedConstraints}
        with
        | ex ->
            printfn $"DEBUG: Error in addDetailedErrorMsg: {ex.Message}"
            // Return original spec if component not found
            spec

    // Check whether a param slot meets its constraints
    let checkSlot (slot: ParamSlot) (spec: ConstrainedExpr) = 
        match slot.CompSlot with
        | CustomCompParam _ ->
            // Get custom component and corresponding loaded component
            let comp = getComponentById model sheetName slot.CompId
            let customComponent = 
                match comp.Type with
                | Custom c ->  c
                | _ -> failwithf "Custom component must have custom component type"
            let customLC = getLoadedComponent (Some customComponent.Name) model
            
            // Create updated set of integer constant bindings for custom component
            let customBindings = 
                customComponent.ParameterBindings
                |> Option.defaultValue Map.empty
                |> Map.map (fun _ expr -> ParameterTypes.evaluateParamExpression updatedBindings expr)
                |> Map.fold (fun acc key result -> 
                    match result with 
                    | Ok value -> Map.add key (PInt value) acc
                    | Error _ -> acc) Map.empty

            let defaultBindings = customLC |> get defaultBindingsOfLC_ |> Option.defaultValue Map.empty
            let mergedBindings = Helpers.mapUnion customBindings defaultBindings

            // Check all slots inside custom component
            checkAllCompSlots model customComponent.Name mergedBindings
            |> Result.mapError (fun err -> $"{comp.Label}.{err}")

        | SheetParam _ -> failwithf "Sheet parameter cannot be slot in a component"
        | _ -> checkConstraints updatedBindings spec

    let paramSlots =
        model
        |> getLoadedComponent (Some sheetName)
        |> fun lc -> lc |> get paramSlotsOfLC_ |> Option.defaultValue Map.empty

    printfn $"DEBUG: Found {paramSlots.Count} parameter slots"
    paramSlots |> Map.iter (fun slot spec -> 
        printfn $"DEBUG: ParamSlot - CompId: {slot.CompId}, CompSlot: {slot.CompSlot}, Expression: {ParameterTypes.renderParamExpression spec.Expression 0}")

    // Check all component slots for broken constraints, and return first error found
    let resultsWithErrorHandling =
        paramSlots
        |> Map.toList
        |> List.map (fun (slot, spec) ->
            try
                let detailedSpec = addDetailedErrorMsg slot spec
                let result = checkSlot slot detailedSpec
                Some result
            with
            | ex ->
                printfn $"DEBUG: Skipping slot {slot.CompId} due to error: {ex.Message}"
                None)
        |> List.choose id
        |> List.filter Result.isError

    match resultsWithErrorHandling with
    | [] -> Ok ()
    | firstError :: _ -> firstError


/// Use sheet component update functions to perform updates
let updateComponent dispatch model slot value =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
    let compId = ComponentId comp.Id

    // Update component slot value
    match slot.CompSlot with
    | Buswidth | IO _ -> model.Sheet.ChangeWidth sheetDispatch compId value 
    | NGateInputs -> 
        match comp.Type with
        | GateN (gateType, _) -> model.Sheet.ChangeGate sheetDispatch compId gateType value
        | MergeN _ -> model.Sheet.ChangeMergeN sheetDispatch compId value
        | SplitN (_, widths, lsbs) -> 
            // For SplitN, NGateInputs controls number of outputs
            let changeWidths (widths: int list) (newNumInputs: int) (defaultVal: int) = 
                match widths.Length with
                | n when n > newNumInputs -> widths[..(newNumInputs-1)]
                | n when n < newNumInputs -> List.append widths (List.init (newNumInputs-n) (fun _ -> defaultVal)) 
                | _ -> widths
            let changeLsbs (lsbs: int list) (widths: int list) (newNumInputs: int) = 
                match lsbs.Length with
                | n when n > newNumInputs -> lsbs[..(newNumInputs-1)]
                | n when n < newNumInputs ->
                    let msbs = List.map2 (fun lsb width -> lsb + width - 1) lsbs widths
                    List.append lsbs (List.init (newNumInputs-n) (fun x -> x + (List.max msbs) + 1)) 
                | _ -> lsbs
            let newWidths = changeWidths widths value 1
            let newLsbs = changeLsbs lsbs widths value
            model.Sheet.ChangeSplitN sheetDispatch compId value newWidths newLsbs
        | _ -> failwithf $"NGateInputs cannot be used with component type {comp.Type}"
    | CustomCompParam _ ->
        // Custom component parameters require special handling
        // TODO: updateCustomCompParam is defined later in file, handle this differently
        failwithf "Custom component parameters update not yet implemented in updateComponent"
    | SheetParam _ ->
        failwithf "Sheet parameters cannot be updated via updateComponent"
    | ParameterTypes.LsbBitNumber ->
        model.Sheet.ChangeLSB sheetDispatch compId (bigint value)
    | ParameterTypes.DefaultValue ->
        model.Sheet.ChangeInputValue sheetDispatch compId (bigint value)

    // Update most recent bus width
    match slot.CompSlot, comp.Type with
    | Buswidth, SplitWire _ | Buswidth, BusSelection _ | Buswidth, Constant1 _ -> ()
    | Buswidth, _ | IO _, _ -> dispatch <| ReloadSelectedComponent value
    | _ -> ()


// exprContainsParams has been moved to ParameterTypes module


/// Adds or updates a parameter slot in loaded component param slots
/// Removes the entry if the expression does not contain parameters
let updateParamSlot
    (slot: ParamSlot)
    (exprSpec: ConstrainedExpr)
    (model: Model)
    : Model = 

    let paramSlots = 
        model
        |> get paramSlotsOfModel_
        |> Option.defaultValue Map.empty

    let newParamSlots =
        match ParameterTypes.exprContainsParams exprSpec.Expression with
        | true  -> Map.add slot exprSpec paramSlots
        | false -> Map.remove slot paramSlots

    set paramSlotsOfModel_ newParamSlots model


/// Add the parameter information from a newly created component to paramSlots
let addParamComponent
    (newCompSpec: NewParamCompSpec)
    (dispatch: Msg -> Unit)
    (compId: CommonTypes.ComponentId)
    : Unit =

    let compIdStr =
        match compId with
        | ComponentId txt -> txt
    
    let slot = {CompId = compIdStr; CompSlot = newCompSpec.CompSlot}
    let exprSpec = {
        Expression = newCompSpec.Expression
        Constraints = newCompSpec.Constraints
    }

    updateParamSlot slot exprSpec |> UpdateModel |> dispatch


/// Create a generic input field which accepts and parses parameter expressions
/// Validity of inputs is checked by parser
/// Specific constraints can be passed by callee
let paramInputField
    (model: Model)
    (prompt: string)
    (defaultValue: int)
    (currentValue: Option<int>)
    (constraints: ParamConstraint list)
    (comp: Component option)
    (compSlotName: CompSlotName)
    (dispatch: Msg -> unit)
    : ReactElement =

    let onChange inputExpr = 
        let paramBindings =
            model
            |> get defaultBindingsOfModel_
            |> Option.defaultValue Map.empty

        // Only return first violated constraint
        let checkConstraints expr =
            let exprSpec = {Expression = expr; Constraints = constraints}
            match evaluateConstraints paramBindings [exprSpec] dispatch with
            | Ok () -> Ok ()
                // Error (ParameterTypes.renderParamExpression expr)
            | Error (firstConstraint :: _) ->
                match firstConstraint with
                | MinVal (_, err) | MaxVal (_, err) -> Error err 
            | Error _ -> failwithf "Cannot have error list with no elements"

        let exprResult = ParameterTypes.parseExpression inputExpr
        let newVal = Result.bind (ParameterTypes.evaluateParamExpression paramBindings) exprResult
        let constraintCheck = Result.bind checkConstraints exprResult

        // Either update component or prepare creation of new component
        let useExpr expr value =
            // Update PopupDialogInfo for new component creation and error messages
            let newCompSpec = {
                CompSlot = compSlotName;
                Expression = expr;
                Constraints = constraints;
                Value = value;
            }
            dispatch <| AddPopupDialogParamSpec (compSlotName, Ok newCompSpec)
            match comp with
            | Some c ->
                // Update existing component
                let exprSpec = {Expression = expr; Constraints = constraints}
                let slot = {CompId = c.Id; CompSlot = compSlotName}
                updateComponent dispatch model slot value
                dispatch <| UpdateModel (updateParamSlot slot exprSpec)
            | None -> ()

        match newVal, constraintCheck, exprResult with
        | Ok value, Ok (), Ok expr -> useExpr expr value
        | Error err, _, _ 
        | _, Error err, _ -> dispatch <| AddPopupDialogParamSpec (compSlotName, Error err)
        | _ -> failwithf "Value cannot exist with invalid expression"

    let slots = model |> getCurrentSheet |> getParamSlots
    let inputString = 
        match comp with
        | Some c ->
            let key = {CompId = c.Id; CompSlot = compSlotName}
            if Map.containsKey key slots then
                ParameterTypes.renderParamExpression slots[key].Expression 0 // Or: Some (Map.find key slots)
            else
                currentValue |> Option.defaultValue defaultValue |> string
        | None -> currentValue |> Option.defaultValue defaultValue |> string
    
    let errText = 
        model.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind compSlotName
        |> Option.map (
            function
            | Ok _ -> "" 
            | Error err -> err
        )
        |> Option.defaultValue ""

    // Field name, input box, and potential error message
    Field.div [] [
        Label.label [] [str prompt]
        Field.div [Field.Option.HasAddons] [
            Control.div [] [
                Input.text [
                    if errText <> "" then
                        Input.Option.CustomClass "is-danger"
                    Input.Props [
                        OnPaste preventDefault
                        SpellCheck false
                        Name prompt
                        AutoFocus true
                        Style [Width "200px"]
                    ]
                    Input.DefaultValue <| inputString
                    Input.Type Input.Text
                    Input.OnChange (getTextEventValue >> onChange)
                ]
            ]
            if currentValue.IsSome && string currentValue.Value <> inputString then
                Control.p [] [
                    Button.a [Button.Option.IsStatic true] [
                        str (string currentValue.Value)
                    ]
                ]
        ]
        p [Style [Color Red]] [str errText]
    ]


/// Create a generic input field which accepts and parses parameter expressions
/// Validity of inputs is checked by parser
/// Specific constraints can be passed by callee
let paramInputFieldAdvanced
    (model: Model)
    (prompt: string)
    (compSpec: NewParamCompSpec)
    (comp: Component option)
    (dispatch: Msg -> unit)
    : ReactElement =

    let onChange inputExpr = 
        // Get parameter bindings and name of current sheet
        let paramBindings = model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty
        let sheetName = 
            model.CurrentProj
            |> Option.get
            |> fun proj -> proj.OpenFileName

        // Check that the expression satisfies all the required constraints
        let applyConstraints expr value =
            match compSpec.CompSlot with
            | SheetParam param ->   // Check all slots on current sheet
                match expr with
                | PInt _ -> 
                    let editedBindings = Map.add param (PInt value) paramBindings
                    checkAllCompSlots model sheetName editedBindings
                | _ -> Error "Parameter value must be a constant"
            | CustomCompParam param ->  // Check all slots in custom component
                let editedBindings = Map.empty |> Map.add param (PInt value)
                let paramComp = Option.get comp
                let customComp = 
                    match paramComp.Type with
                    | Custom cc -> cc
                    | _ -> failwithf "Only custom component can have bindings"
                checkAllCompSlots model customComp.Name editedBindings
                |> Result.mapError (fun err -> $"{paramComp.Label}.{err}")
            | _ ->  // Only need to check slot of selected component
                let exprSpec = {Expression = expr; Constraints = compSpec.Constraints}
                checkConstraints paramBindings exprSpec

        // Either update component or prepare creation of new component
        let useExpr expr value =
            // Update PopupDialogInfo for new component creation and error messages
            let newCompSpec = {compSpec with Expression = expr; Value = value}
            dispatch <| AddPopupDialogParamSpec (compSpec.CompSlot, Ok newCompSpec)
            match comp with
            | Some c ->
                // Update existing component
                let exprSpec = {Expression = expr; Constraints = compSpec.Constraints}
                let slot = {CompId = c.Id; CompSlot = compSpec.CompSlot}
                // TODO: Use a flag to control whether the component auto-updates
                updateComponent dispatch model slot value
                dispatch <| UpdateModel (updateParamSlot slot exprSpec)
            | None -> ()

        // Parse, evaluate, and check constraints of input expression
        let parsedExpr = ParameterTypes.parseExpression inputExpr
        let newVal = Result.bind (ParameterTypes.evaluateParamExpression paramBindings) parsedExpr
        let constraintCheck = 
            match parsedExpr, newVal with
            | Ok expr, Ok value -> applyConstraints expr value
            | Error err, _ | _, Error err -> Error err 

        match newVal, constraintCheck, parsedExpr with
        | Ok value, Ok (), Ok expr -> useExpr expr value
        | Error err, _, _ | _, Error err, _ ->
            dispatch <| AddPopupDialogParamSpec (compSpec.CompSlot, Error err)
        | _ -> failwithf "Value cannot exist with invalid expression"

    // Get the input expression and error message as strings    
    let inputResult = 
        model.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind compSpec.CompSlot

    let boxValue, exprText, errText =
        match inputResult with
        | Some (Ok spec) -> spec.Value, ParameterTypes.renderParamExpression spec.Expression 0, ""
        | Some (Error err) -> compSpec.Value, string compSpec.Value, err
        | None -> compSpec.Value, string compSpec.Value, ""

    // Prompt, input field, potential error message, and result box
    Field.div [] [
        Label.label [] [str prompt]
        Field.div [Field.Option.HasAddons] [
            Control.div [] [
                Input.text [
                    if errText <> "" then
                        Input.Option.CustomClass "is-danger"
                    Input.Props [
                        OnFocus (getTextEventValue >> onChange)
                        OnPaste preventDefault
                        SpellCheck false
                        Name prompt
                        AutoFocus true
                        Style [Width "200px"]
                    ]
                    Input.DefaultValue <| exprText
                    Input.Type Input.Text
                    Input.OnChange (getTextEventValue >> onChange)
                ]
            ]
            if errText = "" && string boxValue <> exprText then
                Control.p [] [
                    Button.a [Button.Option.IsStatic true] [
                        str (string boxValue)
                    ]
                ]
        ]
        p [Style [Color Red]] [str errText]
    ]


/// True if parameter input field for given slot has valid input
let paramInputIsValid (slot: CompSlotName) (model: Model): bool =
    model.PopupDialogData.DialogState
    |> function
       | Some specs -> Map.find slot specs |> Result.isOk
       | None -> failwithf "Dialog state must exist for input box"


/// Get the component specification from a parameter input field
/// Can only be called once it is known that the input expression meets constraints
let getParamFieldSpec (slotName: CompSlotName) (model: Model): NewParamCompSpec =
    let inputFieldDialog = 
        match model.PopupDialogData.DialogState with
        | Some inputSpec -> Map.find slotName inputSpec
        | None -> failwithf "Param input field must set new param info"
    
    match inputFieldDialog with
    | Ok paramSpec -> paramSpec
    | Error err -> failwithf $"Failed to extract expression due to error '{err}'"


/// Config for a parameter popup dialog box
type PopupConfig = {
    Title: string
    Prompt: string
    Button: string
}


/// Create a popup box with a parameter input field
let paramPopupBox
    (text: PopupConfig)
    (compSpec: NewParamCompSpec)
    (comp: Component option)
    (buttonAction: Model -> Unit)
    (dispatch: Msg -> Unit)
    : Unit =

    let inputField model' = paramInputFieldAdvanced model' text.Prompt compSpec comp dispatch

    // Disabled if any constraints are violated
    let isDisabled model' = not <| paramInputIsValid compSpec.CompSlot model'

    // Create parameter input field
    dispatch <| AddPopupDialogParamSpec (compSpec.CompSlot, Ok compSpec)
    dialogPopup text.Title inputField text.Button buttonAction isDisabled [] dispatch


/// Update the values of all parameterised components with a new set of bindings
/// This can only be called after the validity and constraints of all
/// expressions are checked
let updateComponents
    (newBindings: ParamBindings)
    (model: Model)
    (dispatch: Msg -> Unit)
    : Unit =

    let evalExpression expr =
        match ParameterTypes.evaluateParamExpression newBindings expr with
        | Ok value -> value
        | Error _ ->  failwithf "Component update cannot have invalid expression"

    model
    |> get paramSlotsOfModel_
    |> Option.defaultValue Map.empty
    |> Map.map (fun _ expr -> evalExpression expr.Expression)
    |> Map.iter (updateComponent dispatch model)


/// Update a custom component parameter binding
let updateCustomCompParam
    (model: Model)
    (toplevelBindings: ParamBindings)
    (slot: ParamSlot)
    (expr: ParamExpression)
    (dispatch: Msg -> Unit)
    : Unit =

    /// Update a custom component with new bindings and I/O widths
    let createUpdatedComp
        (labelToWidth: Map<string, int>)
        (newBindings: ParamBindings)
        (custom: CustomComponentType)
        (comp: Component)
        : Component =

        let updateLabel (label, width) =
            let newWidth = Map.tryFind label labelToWidth |> Option.defaultValue width
            label, newWidth

        let updatedCustom = {
            custom with 
                InputLabels = List.map updateLabel custom.InputLabels
                OutputLabels = List.map updateLabel custom.OutputLabels
                ParameterBindings = Some newBindings
        }
        {comp with Type = Custom updatedCustom}

    // Get custom component from component ID
    let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
    let custom =
        match comp.Type with
        | Custom c -> c
        | other -> failwithf $"Custom component cannot have type '{other}'"
    
    // Find corresponding loaded component for custom component
    let customLC = 
        match model.CurrentProj with
        | None -> failwithf "Must have open project"
        | Some proj ->
            proj.LoadedComponents
            |> List.find (fun lc -> lc.Name = custom.Name)

    let lcParamInfo =
        match customLC.LCParameterSlots with
        | Some paramInfo -> paramInfo
        | None -> failwithf "Parameterised custom component must have parameter info"

    let paramName = 
        match slot.CompSlot with
        | CustomCompParam param -> param
        | _ -> failwithf "Custom component slot must have CustomCompParam type"

    // New bindings for custom component
    let newBindings =
        custom.ParameterBindings
        |> Option.defaultValue Map.empty
        |> Map.add paramName expr

    // Evaluated bindings for custom component
    let constantBindings = 
        newBindings
        |> Map.map (fun _ expr -> 
            match ParameterTypes.evaluateParamExpression toplevelBindings expr with
            | Ok value -> PInt value
            | Error _ -> PInt 0) // Default fallback

    // Merge default bindings of custom component with modified binding values
    let mergedBindings =
        Helpers.mapUnion constantBindings lcParamInfo.DefaultBindings

    // Create a map of IO label names on the custom component to their widths
    let ioLabelToWidth = 
        lcParamInfo.ParamSlots
        |> Map.toList
        |> List.choose (fun (paramSlot, expr) -> 
            match ParameterTypes.evaluateParamExpression mergedBindings expr.Expression with
            | Ok value ->
                // Only keep IO port slots
                match paramSlot.CompSlot with
                | IO label -> Some (label, value)
                | _ -> None 
            | Error _ -> None)
        |> Map.ofList

    // Update custom component and its symbol displayed on sheet    
    let newComponent = createUpdatedComp ioLabelToWidth newBindings custom comp
    (ComponentId comp.Id, comp, newComponent.Type)
    |> SymbolT.ChangeCustom
    |> BusWireT.Symbol
    |> SheetT.Wire
    |> Sheet
    |> dispatch

    // Do bus width inference
    let sheetDispatch msg = dispatch <| Sheet msg
    model.Sheet.DoBusWidthInference sheetDispatch
    

/// Updates the LCParameterSlots DefaultParams section.
type UpdateInfoSheetChoise = 
    | DefaultParams of string * int * bool
    | ParamSlots of ParamSlot * ParameterTypes.ParamExpression * ParamConstraint list


let updateInfoSheetDefaultParams (currentSheetInfo:option<ParameterTypes.ParameterDefs>) (paramName: string) (value: int) (delete:bool)=
    if delete then
        match currentSheetInfo with
        | Some infoSheet -> 
            let newDefaultParams = infoSheet.DefaultBindings |> Map.remove (ParamName paramName)
            let currentSheetInfo = {infoSheet with DefaultBindings = newDefaultParams}
            Some currentSheetInfo
        | None -> None
    else
    match currentSheetInfo with
    | Some infoSheet -> 
        let newDefaultParams = infoSheet.DefaultBindings|> Map.add (ParamName paramName) (PInt value)
        let currentSheetInfo = {infoSheet with DefaultBindings = newDefaultParams}
        Some currentSheetInfo
    | None -> 
        let currentSheetInfo = {DefaultBindings= Map.ofList [(ParamName paramName, PInt value)]; ParamSlots= Map.empty}
        Some currentSheetInfo


let updateInfoSheetParamSlots (currentSheetInfo:option<ParameterTypes.ParameterDefs>) (paramSlot: ParameterTypes.ParamSlot) (expression: ParameterTypes.ParamExpression) (constraints: ParameterTypes.ParamConstraint list) =
    match currentSheetInfo with
    | Some infoSheet -> 
        let newParamSlots = infoSheet.ParamSlots |> Map.add paramSlot {Expression = expression; Constraints = constraints}
        let currentSheetInfo = {infoSheet with ParamSlots = newParamSlots}
        Some currentSheetInfo
    | None -> 
        let currentSheetInfo = {DefaultBindings= Map.empty; ParamSlots = Map.ofList [paramSlot, {Expression = expression; Constraints = constraints}]}
        Some currentSheetInfo


let updateParameter (project: CommonTypes.Project) (model: Model) =
    {model with CurrentProj = Some project}


let getParamsSlot (currentSheet: CommonTypes.LoadedComponent) =
    let getter = CommonTypes.lcParameterSlots_ >?> ParameterTypes.paramSlots_
    match currentSheet.LCParameterSlots with
    | Some _ -> currentSheet ^. getter
    | None -> None


/// This function can be used to update the DefaultParams or ParamSlots in the LCParameterSlots of a sheet based on the choise
/// Use case will be either when we want to add, edit or delete the sheet parameter or when we want to add a new component to the sheet
let modifyInfoSheet (project: CommonTypes.Project) (choise: UpdateInfoSheetChoise) dispatch=
    
    let currentSheet = project.LoadedComponents
                                   |> List.find (fun lc -> lc.Name = project.OpenFileName)
    let updatedSheet = {currentSheet with LCParameterSlots = 
                                                        match choise with
                                                            | DefaultParams (paramName, value, delete) -> updateInfoSheetDefaultParams currentSheet.LCParameterSlots paramName value delete
                                                            | ParamSlots (paramSlot, expression, constraints) -> updateInfoSheetParamSlots currentSheet.LCParameterSlots paramSlot expression constraints}
    let updatedComponents = project.LoadedComponents
                            |> List.map (
                                fun lc ->
                                    if lc.Name = project.OpenFileName
                                    then updatedSheet
                                    else lc
                                )
    let newProject = {project with LoadedComponents = updatedComponents}
    updateParameter newProject |> UpdateModel |> dispatch


/// Creates a popup that allows a parameter integer value to be edited.
let editParameterBox model parameterName dispatch   = 
    // Get current value
    let currentVal =
        model
        |> get defaultBindingsOfModel_
        |> Option.defaultValue Map.empty
        |> Map.find (ParamName parameterName)
        |> function
           | PInt intVal -> intVal
           | _ -> failwithf "Edit parameter box only supports constants"

    // Treat parameter binding as a slot to enable input error checking
    let slot = SheetParam <| ParamName parameterName

    // Update parameter bindings and components
    let buttonAction model' =
        // Get new parameter value from input field
        let compParamSpec = getParamFieldSpec slot model'
        let newValue = compParamSpec.Value

        // Update bindings
        let newBindings =
            model'
            |> get defaultBindingsOfModel_
            |> Option.defaultValue Map.empty
            |> Map.add (ParamName parameterName) (PInt newValue) 
        dispatch <| UpdateModel (set defaultBindingsOfModel_ newBindings)

        // Update components
        updateComponents newBindings model' dispatch 
        dispatch ClosePopup

    let popupConfig = {
        Title = "Edit parameter value"
        Prompt = $"New value for parameter {parameterName}"
        Button = "Set value"
    }

    let paramSpec = {
        CompSlot = slot
        Expression = PInt currentVal
        Constraints = []
        Value = currentVal
    }

    paramPopupBox popupConfig paramSpec None buttonAction dispatch


/// Create a popup that allows a parameter with an integer value to be added
let addParameterBox dispatch =
    // Dialog popup config
    let title = "Add new parameter"
    let textPrompt _ = Field.div [] [str "Parameter name"]
    let hint = "Enter a name"
    let intPrompt _ = Field.div [] [str "Parameter value"]
    let defaultVal = 1
    let buttonText = "Set value"

    let body = dialogPopupBodyTextAndInt textPrompt hint intPrompt defaultVal dispatch

    // Create empty parameter info if currently None
    let initParamInfo model =
        let loadedComponent = 
            model
            |> get openLoadedComponentOfModel_
            |> function
               | Some lc -> lc
               | None -> failwithf "Must have open sheet" 

        let defaultInfo =  Some {DefaultBindings = Map.empty; ParamSlots = Map.empty}
        let updatedLC = 
            match loadedComponent.LCParameterSlots with
            | Some _ -> loadedComponent
            | None -> {loadedComponent with LCParameterSlots = defaultInfo}
        dispatch <| UpdateModel (set openLoadedComponentOfModel_ updatedLC)

    // Update the parameter value then close the popup
    let buttonAction model =
        initParamInfo model // Needed before adding first parameter
        let newParamName = getText model.PopupDialogData
        let newValue = getInt model.PopupDialogData

        // Update bindings
        let newBindings =
            model 
            |> get defaultBindingsOfModel_
            |> Option.defaultValue Map.empty
            |> Map.add (ParamName newParamName) (PInt newValue)
        dispatch <| UpdateModel (set defaultBindingsOfModel_ newBindings)
        dispatch ClosePopup

    // Parameter names can only contain letters and numbers
    let isDisabled model = 
        let newParamName = getText model.PopupDialogData
        not (Regex.IsMatch(newParamName, "^[a-zA-Z][a-zA-Z0-9]*$"))

    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let deleteParameterBox model parameterName dispatch = 
    let newBindings =
        model
        |> get defaultBindingsOfModel_
        |> Option.defaultValue Map.empty
        |> Map.remove (ParamName parameterName)

    dispatch <| UpdateModel (set defaultBindingsOfModel_ newBindings)


/// create a popup to edit in the model a custom component parameter binding
/// TODO - maybe comp should be a ComponentId with actual component looked up from model for safety?
let editParameterBindingPopup
    (model: Model)
    (paramName: ParamName)
    (comp: Component)
    (dispatch: Msg -> Unit)
    : Unit = 

    let customComponent =
        match comp.Type with
        | Custom c -> c
        | _ -> failwithf "Only custom components can have parameter bindings"

    let defaultVal =
        model
        |> getLoadedComponent (Some customComponent.Name)
        |> fun lc -> lc |> get defaultBindingsOfLC_ |> Option.defaultValue Map.empty
        |> Map.find paramName
        |> function
           | PInt value -> value
           | _ -> failwithf "Default bindings must be constants"

    let currentExpr =
        customComponent.ParameterBindings
        |> Option.defaultValue Map.empty
        |> Map.tryFind paramName
        |> Option.defaultValue (PInt defaultVal)

    // Popup dialog config
    let slot = CustomCompParam paramName

    // Update custom component when button is clicked
    let buttonAction model' =
        // Get new parameter expression from input field
        let paramBindings = model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty
        let newSpec = getParamFieldSpec slot model'

        // Add new binding to param slots of current sheet
        let compSlot = {CompId = comp.Id; CompSlot = newSpec.CompSlot}
        let exprSpec = {
            Expression = newSpec.Expression
            Constraints = newSpec.Constraints
        }
        dispatch <| UpdateModel (updateParamSlot compSlot exprSpec)

        // Update custom component with new parameter value
        updateCustomCompParam model' paramBindings compSlot newSpec.Expression dispatch
        dispatch ClosePopup

    let popupConfig = {
        Title = "Edit parameter value"
        Prompt = $"New value for parameter {paramName}"
        Button = "Set value"
    }

    let currentSpec = {
        CompSlot = slot
        Expression = currentExpr
        Constraints = []
        Value = defaultVal
    }

    paramPopupBox popupConfig currentSpec (Some comp) buttonAction dispatch


/// UI to display and manage parameters for a design sheet.
/// TODO: add structural abstraction.
let private makeParamsField model (comp:LoadedComponent) dispatch =
    let sheetDefaultParams = getDefaultParams comp
    match sheetDefaultParams.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameters" ]
            p [] [str "No parameters have been added to this sheet." ]   
            br [] 
            Button.button 
                            [ Fulma.Button.OnClick(fun _ -> addParameterBox dispatch)
                              Fulma.Button.Color IsInfo
                            ] 
                [str "Add Parameter"]
            ]
    | false ->
    
        div [] [
            Label.label [] [str "Parameters"]
            p [] [str "These parameters have been added to this sheet." ]
            br []
            Table.table [
                        Table.IsBordered
                        Table.IsNarrow
                        Table.IsStriped
                        ] [
                thead [] [
                    tr [] [
                        th [] [str "Parameter"]
                        th [] [str "Value"]
                        th [] [str "Action"]
                    ]
                ]
                tbody [] (
                    sheetDefaultParams |> Map.toList |> List.map (fun (key, value) ->
                        let paramName =
                            match key with 
                            | ParameterTypes.ParamName s -> s
                        let paramVal = ParameterTypes.renderParamExpression value 0
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramVal]
                            td [] [
                                Button.button 
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBox model (paramName) dispatch)
                                      Fulma.Button.Color IsInfo
                                    ] 
                                    [str "Edit"]
                                Button.button 
                                    [ Fulma.Button.OnClick(fun _ -> deleteParameterBox model (paramName) dispatch )
                                      Fulma.Button.Color IsDanger
                                    ] 
                                    [str "Delete"]
                                ]
                            ]
                        )
                    )
                ]
            Button.button 
                [ Fulma.Button.OnClick(fun _ -> addParameterBox dispatch)
                  Fulma.Button.Color IsInfo
                ]
                [str "Add Parameter"]
        ]

/// Evaluate parameter expression using parameter bindings - exposed for external use

/// Helper function for simulation: resolve parameter expressions for a component
/// Returns the component type with resolved parameter values
// Create prisms for component type parameter updates using the existing Optics library
let buswidthPrism : Prism<ComponentType, int> =
    Prism.create
        (function
            | Viewer w | Input w | Output w 
            | NbitsAdder w | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w
            | NbitsAnd w | NbitsNot w | NbitsOr w | NbitSpreader w | SplitWire w
            | Register w | RegisterE w | Counter w | CounterNoLoad w 
            | CounterNoEnable w | CounterNoEnableLoad w -> Some w
            | BusCompare1 (w, _, _) | Constant1 (w, _, _) | BusSelection (w, _) 
            | NbitsXor (w, _) | Shift (w, _, _) | BusCompare (w, _) 
            | Input1 (w, _) | Constant (w, _) -> Some w
            | _ -> None)
        (fun w compType ->
            match compType with
            | Viewer _ -> Viewer w
            | BusCompare1 (_, cv, dt) -> BusCompare1 (w, cv, dt)
            | BusSelection (_, lsb) -> BusSelection (w, lsb)
            | Constant1 (_, cv, dt) -> Constant1 (w, cv, dt)
            | NbitsAdder _ -> NbitsAdder w
            | NbitsAdderNoCin _ -> NbitsAdderNoCin w
            | NbitsAdderNoCout _ -> NbitsAdderNoCout w
            | NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout w
            | NbitsXor (_, op) -> NbitsXor (w, op)
            | NbitsAnd _ -> NbitsAnd w
            | NbitsNot _ -> NbitsNot w
            | NbitsOr _ -> NbitsOr w
            | NbitSpreader _ -> NbitSpreader w
            | SplitWire _ -> SplitWire w
            | Register _ -> Register w
            | RegisterE _ -> RegisterE w
            | Counter _ -> Counter w
            | CounterNoLoad _ -> CounterNoLoad w
            | CounterNoEnable _ -> CounterNoEnable w
            | CounterNoEnableLoad _ -> CounterNoEnableLoad w
            | Shift (_, sw, st) -> Shift (w, sw, st)
            | BusCompare (_, cv) -> BusCompare (w, cv)
            | Input _ -> Input w
            | Input1 (_, dv) -> Input1 (w, dv)
            | Output _ -> Output w
            | Constant (_, cv) -> Constant (w, cv)
            | _ -> compType)

let ngateInputsPrism : Prism<ComponentType, int> =
    Prism.create
        (function GateN (_, n) -> Some n | _ -> None)
        (fun n -> function GateN (gt, _) -> GateN (gt, n) | t -> t)

let ioPortPrism : Prism<ComponentType, int> =
    Prism.create
        (function Input1 (w, _) | Output w -> Some w | _ -> None)
        (fun w -> function 
            | Input1 (_, dv) -> Input1 (w, dv) 
            | Output _ -> Output w 
            | t -> t)

let resolveParametersForComponent 
    (paramBindings: ParamBindings) 
    (paramSlots: Map<ParamSlot, ConstrainedExpr>) 
    (comp: Component) 
    : Result<Component, string> =
    
    let compIdStr = comp.Id
    let relevantSlots = 
        paramSlots 
        |> Map.filter (fun slot _ -> slot.CompId = compIdStr)

    if Map.isEmpty relevantSlots then
        Ok comp
    else
        relevantSlots
        |> Map.toList
        |> List.fold 
            (fun (currentType, errorOpt) (slot, constrainedExpr) ->
                match errorOpt with
                | Some _ -> (currentType, errorOpt)
                | None ->
                    match ParameterTypes.evaluateParamExpression paramBindings constrainedExpr.Expression with
                    | Ok evaluatedValue -> 
                        let newType =
                            match slot.CompSlot with
                            | Buswidth -> currentType |> (evaluatedValue ^= buswidthPrism)
                            | NGateInputs -> currentType |> (evaluatedValue ^= ngateInputsPrism)
                            | IO _ -> currentType |> (evaluatedValue ^= ioPortPrism)
                            | _ -> currentType
                        (newType, None)
                    | Error err -> (currentType, Some err)
            )
            (comp.Type, None)
        |> function
            | (_, Some err) -> Error err
            | (updatedType, None) -> Ok { comp with Type = updatedType }

/// Update LoadedComponent port labels after parameter resolution
let updateLoadedComponentPorts (loadedComponent: LoadedComponent) : LoadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some paramSlots when not (Map.isEmpty paramSlots.ParamSlots) ->
        // Apply parameter resolution to get updated port labels
        let (comps, conns) = loadedComponent.CanvasState
        let resolvedComps = 
            comps |> List.map (fun comp ->
                match resolveParametersForComponent paramSlots.DefaultBindings paramSlots.ParamSlots comp with
                | Ok resolvedComp -> resolvedComp
                | Error _ -> comp // Keep original on error
            )
        let resolvedCanvas = (resolvedComps, conns)
        let newInputLabels = CanvasExtractor.getOrderedCompLabels (Input1 (0, None)) resolvedCanvas
        let newOutputLabels = CanvasExtractor.getOrderedCompLabels (Output 0) resolvedCanvas
        
        { loadedComponent with 
            InputLabels = newInputLabels
            OutputLabels = newOutputLabels }
    | _ -> loadedComponent

/// Update a custom component with new I/O component widths.
/// Used when these chnage as result of parameter changes.
let updateCustomComponent (labelToEval: Map<string, int>) (newBindings: ParamBindings) (comp: Component) : Component =
    let updateLabels labels =
        labels |> List.map (fun (label, width) ->
            match Map.tryFind label labelToEval with
            | Some newWidth when newWidth <> width -> (label, newWidth) // Update width if changed
            | _ -> (label, width) // Keep the same if unchanged
        )
    
    match comp.Type with
    | Custom customComponent ->
        let updatedCustom = { customComponent with 
                                    InputLabels = updateLabels customComponent.InputLabels
                                    OutputLabels = updateLabels customComponent.OutputLabels
                                    ParameterBindings = Some newBindings }
        { comp with Type = Custom updatedCustom }
    | _ -> comp


/// UI component for custom component definition of parameter bindings
let makeParamBindingEntryBoxes model (comp:Component) (custom:CustomComponentType) dispatch =
    let ccParams = 
        match custom.ParameterBindings with
        | Some bindings -> bindings
        | None -> Map.empty
    
    let lcDefaultParams =
        match model.CurrentProj with
        | Some proj -> 
            let lcName = List.tryFind (fun c -> custom.Name = c.Name) proj.LoadedComponents
            match lcName with
            | Some lc -> getDefaultParams lc
            | None -> Map.empty
        | None -> Map.empty

    let mergedParamBindings : ParamBindings =
        lcDefaultParams
        |> Map.map (fun key value -> 
            match Map.tryFind key ccParams with
            | Some ccValue -> ccValue // Overwrite if key exists in cc
            | None -> value // use loaded component value if key does not exist in cc
            )

    match mergedParamBindings.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameters" ]
            p [] [str "This component does not use any parameters." ]
        ]   
    | false ->
        div [] [
            Label.label [] [str "Parameters"]
            p [] [str "This component uses the following parameters." ]
            br []
            Table.table [
                        Table.IsBordered
                        Table.IsNarrow
                        Table.IsStriped
                        ] [
                thead [] [
                    tr [] [
                        th [] [str "Parameter"]
                        th [] [str "Value"]
                        th [] [str "Action"]
                    ]
                ]
                tbody [] (
                    mergedParamBindings |> Map.toList |> List.map (fun (key, value) ->
                        let paramName =
                            match key with 
                            | ParameterTypes.ParamName s -> s
                        let paramVal = ParameterTypes.renderParamExpression value 0
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramVal]
                            td [] [
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model (ParamName paramName) comp dispatch)
                                      Fulma.Button.Color IsInfo
                                    ] 
                                    [str "Edit"]
                            ]
                        ]
                    )
                )
            ]
        ]

/// Generate component slots view for design sheet properties panel
/// This is read-only.
let private makeSlotsField (model: ModelType.Model) (comp:LoadedComponent) dispatch = 
    let sheetParamsSlots = getParamsSlot comp

    // Define a function to display PConstraint<int>
    let constraintExpression (constraint': ParamConstraint) =
        match constraint' with
        | MaxVal (expr, err) ->
            div [] [str ("Max: " + ParameterTypes.renderParamExpression expr 0)]
        | MinVal (expr, err) ->
            div [] [str ("Min: " + ParameterTypes.renderParamExpression expr 0)]
    
    let constraintMessage (constraint': ParamConstraint) =
        match constraint' with
            | MaxVal (_, err)  | MinVal (_, err) -> err


    /// UI component to display a single parameterised Component slot definition.
    /// This is read-only.
    let renderSlotSpec (slot: ParamSlot) (expr: ConstrainedExpr) =
        let slotNameStr =
            match slot.CompSlot with
            | Buswidth -> "Buswidth"
            | NGateInputs -> "Num inputs"
            | IO label -> $"Input/output {label}"
            | CustomCompParam param -> $"Parameter binding of {param}"
            | SheetParam _ -> failwithf $"Sheet parameter cannot be slot in a component"
            | ParameterTypes.LsbBitNumber -> "LSB bit number"
            | ParameterTypes.DefaultValue -> "Default value"
        
        let name = if Map.containsKey (ComponentId slot.CompId) model.Sheet.Wire.Symbol.Symbols then
                        string model.Sheet.Wire.Symbol.Symbols[ComponentId slot.CompId].Component.Label
                    else
                        "[Nonexistent]" // TODO deleted component slots aren't removed!
        tr [] [
            td [] [
                b [] [str name] 
                br [] 
                str slotNameStr
            ]
            td [] [str (ParameterTypes.renderParamExpression expr.Expression 0)]
            td [
                Class (Tooltip.ClassName + " " + Tooltip.IsTooltipLeft)
                Tooltip.dataTooltip (List.map constraintMessage expr.Constraints |> String.concat "\n")
            ] (List.map constraintExpression expr.Constraints)
        ]

    /// UI component to display parametrised Component slot definitions 
    /// on the properties panel of a design sheet.
    /// This is read-only - changes can be made via the priperties of the component.
    let slotView (slotMap: ComponentSlotExpr) =
        div [Class "component-slots"] [ 
            label [Class "label"] [ str "Parameterised Components"]
            // br []
            p [] [str "This sheet contains the following parameterised components"]
            br []
            Table.table [
                Table.IsBordered
                Table.IsNarrow
                Table.IsStriped
                ] [
                thead [] [
                    tr [] [
                        th [] [str "Component"]
                        th [] [str "Expression"]
                        th [] [str "Constraint"]
                    ]
                ]
                tbody [] (
                        // slots |> Map.toList |> List.map (fun (slot, expr) -> renderSlotSpec slot expr
                        slotMap |> Map.toList |> List.map (fun (slot, expr) -> renderSlotSpec slot expr)
                    )
                ]
        ]

    match sheetParamsSlots with
        |None ->
            div [] [
                Label.label [] [ str "Parameterised Components" ]
                p [] [str "This sheet does not contain any parameterised." ]    
                ]
        |Some sheetParamsSlots -> slotView sheetParamsSlots

/// UI interface for viewing the parameter expressions of a component
let viewParameters (model: ModelType.Model) dispatch =
    
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [p [] [str $"Currently no parameters added into {comp.Label} sheet." ]    ]    
    | _ -> 
        match model.CurrentProj with
        |Some proj ->
            let sheetName = proj.OpenFileName
            let sheetLdc = proj.LoadedComponents |> List.find (fun ldc -> ldc.Name = sheetName)
            div [] [
            makeParamsField model sheetLdc dispatch
            br []
            makeSlotsField model sheetLdc dispatch]
        |None -> null
