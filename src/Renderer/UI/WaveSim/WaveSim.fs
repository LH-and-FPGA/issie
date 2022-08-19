module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open WaveSimStyle
open WaveSimHelpers
open FileMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType

/// Generates SVG to display values on non-binary waveforms when there is enough space.
/// TODO: Fix this so it does not generate all 500 cycles.
let displayValuesOnWave wsModel (waveValues: WireData list) (transitions: NonBinaryTransition list) : ReactElement list =
    /// Find all clock cycles where there is a NonBinaryTransition.Change
    let changeTransitions =
        transitions
        |> List.indexed
        |> List.filter (fun (_, x) -> x = Change)
        |> List.map (fun (i, _) -> i)

    /// Find start and length of each gap between a Change transition
    let gaps : Gap list =
        // Append dummy transition to end to check final gap length
        changeTransitions @ [Constants.maxLastClk]
        |> List.pairwise
        // Get start of gap and length of gap
        |> List.map (fun (i1, i2) -> {
                Start = i1
                Length = i2 - i1
            }
        )

    gaps
    // Create text react elements for each gap
    |> List.map (fun gap ->
        let waveValue =
            let wd = waveValues[gap.Start]
            valToPaddedString wd.Length  wsModel.Radix (convertWireDataToInt wd)
  

        /// Amount of whitespace between two Change transitions minus the crosshatch
        let availableWidth = (float gap.Length * (singleWaveWidth wsModel)) - 2. * Constants.nonBinaryTransLen
        /// Required width to display one value
        let requiredWidth = DrawHelpers.getTextWidthInPixels (waveValue, Constants.valueOnWaveText)
        /// Width of text plus whitespace between a repeat
        let widthWithPadding = 2. * requiredWidth + Constants.valueOnWavePadding

        // Display nothing if there is not enough space
        if availableWidth < requiredWidth then
            []
        else
            let valueText i =
                text (valueOnWaveProps wsModel i gap.Start widthWithPadding)
                    [ str waveValue ]

            /// Calculate how many times the value can be shown in the space available
            let repeats = int <| availableWidth / widthWithPadding

            [ 0 .. repeats ]
            |> List.map valueText
    )
    |> List.concat

/// Called when InitiateWaveSimulation msg is dispatched
/// and when wave simulator is refreshed.
/// Generates the SVG for a specific waveform.
let generateWaveform (wsModel: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    // Only generate waveforms for selected waves
    if List.contains index wsModel.SelectedWaves then
        let waveform =
            match wave.Width with
            | 0 -> failwithf "Cannot have wave of width 0"
            // Binary waveform
            | 1 ->
                let start = TimeHelpers.getTimeMs ()
                let transitions = calculateBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let wavePoints =
                    List.mapi (binaryWavePoints (singleWaveWidth wsModel) 0) transitions 
                    |> List.concat
                    |> List.distinct

                svg (waveRowProps wsModel)
                    [ polyline (wavePolylineStyle wavePoints) [] ]
                |> TimeHelpers.instrumentInterval "binary waveform" start
            // Non-binary waveform
            | _ ->
                let start = TimeHelpers.getTimeMs ()

                let transitions = calculateNonBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let fstPoints, sndPoints =
                    List.mapi (nonBinaryWavePoints (singleWaveWidth wsModel) 0) transitions 
                    |> List.unzip
                let makePolyline points = 
                    let points =
                        points
                        |> List.concat
                        |> List.distinct
                    polyline (wavePolylineStyle points) []

                let valuesSVG = displayValuesOnWave wsModel wave.WaveValues transitions

                svg (waveRowProps wsModel)
                    (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG)
                |> TimeHelpers.instrumentInterval "nonbinary waveform" start

        {wave with SVG = Some waveform}
    else wave

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

/// Get port names for waves that are from Input ports.
/// Appended to comp.Label
let getInputPortName (compType: ComponentType) (port: InputPortNumber) : string =
    let muxPortName (size: int) : string =
        if port = (InputPortNumber size) then ".SEL"
        else "." + string port

    match compType with
    | Not | BusCompare _ ->
        ".IN"
    | And | Or | Xor | Nand | Nor | Xnor |NbitsNot _ |NbitSpreader _ ->
        ".IN" + string port

    | Mux2 ->
        muxPortName 2
    | Mux4 ->
        muxPortName 4
    | Mux8 ->
        muxPortName 8

    | Decode4 ->
        match port with
        | InputPortNumber 0 -> ".SEL"
        | _ -> ".DATA"

    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ ->
        ""
    | DFF | Register _ ->
        ".D"

    | ROM1 _ | AsyncROM1 _ ->
        ".ADDR"

    | Demux2 | Demux4 | Demux8 ->
        match port with
        | InputPortNumber 0 -> ".DATA"
        | _ -> ".SEL"

    | NbitsXor _ | NbitsAnd _->
        match port with
        | InputPortNumber 0 -> ".P"
        | _ -> ".Q"

    | NbitsAdder _ ->
        match port with
        | InputPortNumber 0 -> ".Cin"
        | InputPortNumber 1 -> ".P"
        | _ -> ".Q"

    | DFFE | RegisterE _ ->
        match port with
        | InputPortNumber 0 -> ".D"
        | _ -> ".EN"

    | RAM1 _ | AsyncRAM1 _ ->
        match port with
        | InputPortNumber 0 -> ".ADDR"
        | InputPortNumber 1 -> ".DIN"
        | _ -> ".WEN"

    | Custom c ->
        "." + fst c.InputLabels[getInputPortNumber port]

    | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | IOLabel -> failwithf "IOLabel should not occur in getInputPortName"
    | MergeWires -> failwithf "MergeWires should not occur in getInputPortName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getInputPortName"
    | BusSelection _ -> failwithf "BusSelection should not occur in getInputPortName"

/// Get names for waves that are from Input ports
let getInputName (comp: NetListComponent) (port: InputPortNumber) : string =
    let portName : string = getInputPortName comp.Type port
    let bitLims : string =
        match comp.Type with
        | Not | BusCompare _ | And | Or | Xor | Nand | Nor | Xnor
        | Mux2 | Mux4 | Mux8 | Decode4 | Demux2 | Demux4 | Demux8
        | DFF | Register _ | DFFE | RegisterE _ |NbitSpreader _ ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsNot w | NbitsAnd w | NbitsAdder w  ->
            bitLimsString (w - 1, 0)

        // TODO: Find the right parameters for RAMs and ROMs.
        | ROM1 _ | AsyncROM1 _ | RAM1 _ | AsyncRAM1 _ ->
            ""

        | Custom c ->
            bitLimsString (snd c.InputLabels[getInputPortNumber port] - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | IOLabel -> failwithf "IOLabel should not occur in getInputName"
        | MergeWires -> failwithf "MergeWires should not occur in getInputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getInputName"
        | BusSelection _ -> failwithf "BusSeleciton should not occur in getInputName"

    comp.Label + portName + bitLims

/// Get port names for waves that are from Output ports
/// Appended to comp.Label
let getOutputPortName (compType: ComponentType) (port: OutputPortNumber) : string =
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ | NbitsXor _ | NbitsNot _  | NbitSpreader _ | NbitsAnd _ ->
        ".OUT"
    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ | IOLabel ->
        ""
    | Demux2 | Demux4 | Demux8 ->
        "." + string port
    | NbitsAdder _ ->
        match port with
        | OutputPortNumber 0 ->
            ".SUM"
        | _ ->
            ".COUT"
    | DFF | DFFE | Register _ | RegisterE _ ->
        ".Q"
    | RAM1 _ | AsyncRAM1 _ | AsyncROM1 _ | ROM1 _ ->
        ".DOUT"
    | Custom c ->
        "." + fst c.OutputLabels[getOutputPortNumber port]

    | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
    | BusSelection _ -> failwithf "BusSeleciton should not occur in getOutputName"

/// Get names for waves that are from Output ports
let getOutputName (comp: NetListComponent) (port: OutputPortNumber) (fastSim: FastSimulation): string =
    let portName = getOutputPortName comp.Type port
    let bitLims =
        match comp.Type with
        | Not | And | Or | Xor | Nand | Nor | Xnor  | BusCompare _
        | Decode4 | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | DFF | DFFE ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsAnd w | NbitsNot w | NbitSpreader w | NbitsAdder w | Register w | RegisterE w ->
            bitLimsString (w - 1, 0)

        | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem ->
            bitLimsString (mem.WordWidth - 1, 0)

        | Custom c ->
            bitLimsString (snd c.OutputLabels[getOutputPortNumber port] - 1, 0)

        | IOLabel ->
            let drivingComp = fastSim.FIOActive[ComponentLabel comp.Label,[]]
            let labelWidth = FastRun.extractFastSimulationWidth fastSim (drivingComp.Id,[]) (OutputPortNumber 0)
            match labelWidth with
            | None ->
                failwithf $"What? Can't find width for IOLabel {comp.Label}$ "
            | Some width ->
                bitLimsString (width - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
        | BusSelection _ -> failwithf "BusSelection should not occur in getOutputName"

    comp.Label + portName + bitLims

/// Get name for a wave. Names are generated from component label, port name, and bit width of wave.
let getName (comp: NetListComponent) (index: WaveIndexT) (fastSim: FastSimulation) : string =
    match index.PortType with
    | PortType.Input -> getInputName comp (InputPortNumber index.PortNumber)
    | PortType.Output -> getOutputName comp (OutputPortNumber index.PortNumber) fastSim

/// Make Wave for each component and port on sheet.
let makeWave (fastSim: FastSimulation) (netList: Map<ComponentId, NetListComponent>) (index: WaveIndexT) (comp: NetListComponent) : Wave =
    let start = TimeHelpers.getTimeMs ()

    let driverComp, driverPort =
        match index.PortType with
        | PortType.Output -> comp, (OutputPortNumber index.PortNumber)
        | PortType.Input ->
            match Map.tryFind (InputPortNumber index.PortNumber) comp.Inputs with
            | Some (Some nlSource) -> netList[nlSource.SourceCompId], nlSource.OutputPort
            | Some None -> failwithf "is there an unconnected input?\n wave: %A\n port: %A %A\n type: %A" comp.Label index.PortType index.PortNumber comp.Type
            | None -> failwithf "InputPortNumber %A not in comp.Inputs" (index.PortNumber)

    let driverId, driverPort = getFastDriver fastSim driverComp driverPort

    let waveValues =
        [ 0 .. Constants.maxLastClk ]
        |> List.map (fun i -> 
            FastRun.extractFastSimulationOutput fastSim i driverId driverPort
            |> function
                | IAlg _ -> 
                    failwithf "what? Algebra in WaveSim waveValues"
                | IData wd -> 
                    wd)

    /// Connections which the wave's port is connected to.
    let conns : ConnectionId list =
        match index.PortType with
        | PortType.Output ->
            List.map (fun x -> x.TargetConnId) netList[index.Id].Outputs[OutputPortNumber index.PortNumber]
        | PortType.Input ->
            match netList[index.Id].Inputs[InputPortNumber index.PortNumber] with
            | Some nlSource -> [nlSource.SourceConnId]
            | None -> []

    {
        WaveId = index
        Type = comp.Type
        CompLabel = comp.Label
        SheetId = []
        Conns = conns
        Driver = {DriverId = driverId; Port = driverPort}
        DisplayName = getName comp index fastSim
        Width =  getFastOutputWidth fastSim.FComps[driverId] driverPort
        WaveValues = waveValues
        SVG = None
    }
    |> TimeHelpers.instrumentInterval "makeWave" start

/// Make wave from Viewer components
let makeViewerWave (fastSim: FastSimulation) (index: WaveIndexT) (viewer: FastComponent) : Wave =
    let driverId, driverPort =
        match Array.head viewer.InputDrivers with
        | Some (fId, opn) -> fId, opn
        | None -> failwithf "Viewer %A has no driver" viewer.FullName

    let waveValues =
        [ 0 .. Constants.maxLastClk ]
        |> List.map (fun i -> 
            FastRun.extractFastSimulationOutput fastSim i driverId driverPort
            |> function
                | IAlg _ -> 
                    failwithf "what? Algebra in WaveSim waveValues"
                | IData wd -> 
                    wd)

    {
        WaveId = index
        Type = viewer.FType
        CompLabel = string viewer.SimComponent.Label
        SheetId = []
        Conns = []
        Driver = {DriverId = driverId; Port = driverPort}
        DisplayName = string viewer.SimComponent.Label
        Width =  getFastOutputWidth fastSim.FComps[driverId] driverPort
        WaveValues = waveValues
        SVG = None
    }

/// Get all simulatable waves from CanvasState. Includes all Input and Output ports.
let getWaves (simData: SimulationData) (reducedState: CanvasState) : Map<WaveIndexT, Wave> =
    let start = TimeHelpers.getTimeMs ()

    let fastSim = simData.FastSim
    let netList = Helpers.getNetList reducedState

    /// Adds all input and output ports from each component.
    /// Removes illegal components (MergeWires, SplitWire, BusSelection).
    let getAllPorts ((id, nlc): (ComponentId * NetListComponent)) : (WaveIndexT * NetListComponent) list =
        match nlc.Type with
        // These types should not appear in the waveform simulator.
        | MergeWires | SplitWire _ | BusSelection _ ->
            []
        | _ ->
            let inputNum = Map.count nlc.Inputs
            let outputNum = Map.count nlc.Outputs

            let getWavesForEachPort (portNum: int) (portType: PortType) =
                [0 .. portNum - 1]
                |> List.map (fun x ->
                    {Id = id; PortType = portType; PortNumber = x}, nlc
                )

            let inputs =
                match nlc.Type with
                | IOLabel -> []
                | _ -> getWavesForEachPort inputNum PortType.Input

            let outputs = getWavesForEachPort outputNum PortType.Output

            List.append inputs outputs

    let ioLabels, otherComps =
        netList
        |> Map.toList
        |> List.partition (fun (_, nlc) -> nlc.Type = IOLabel)

    /// Remove duplicate IOLabels. These occur when e.g. you have an IOLabel connected to an output, and the same
    /// IOLabel driving one or more inputs.
    let ioLabels : (ComponentId * NetListComponent) list = List.distinctBy (fun (_, nlc) -> nlc.Label) ioLabels

    /// Add Viewer waves. This requires a slightly different approach since they are not on the CanvasState.
    let viewerWaves =
        let viewerWavesStart = TimeHelpers.getTimeMs ()
        Map.values fastSim.FComps |> Seq.toList
        |> List.filter (fun fc -> match fc.FType with Viewer _ -> true | _ -> false)
        |> List.map (fun viewer ->
            /// TODO: Should the PortType be Input?
            let index = {Id = viewer.cId; PortType = PortType.Output; PortNumber = 0}
            index, viewer
        )
        |> Map.ofList
        |> Map.map (makeViewerWave fastSim)
        |> TimeHelpers.instrumentInterval "viewerWaves" viewerWavesStart


    List.append ioLabels otherComps
    |> List.collect getAllPorts
    |> TimeHelpers.instrumentInterval "getAllPorts" start
    |> Map.ofList
    |> Map.map (makeWave fastSim netList)
    |> TimeHelpers.instrumentInterval "makeWavePipeline" start

    // Combine the viewer waves to the IOLabel waves and other Component waves
    |> Map.fold (fun vMap key value -> Map.add key value vMap) viewerWaves
    |> TimeHelpers.instrumentInterval "getWaves" start

/// Sets all waves as selected or not selected depending on value of selected
let toggleSelectAll (selected: bool) (wsModel: WaveSimModel) dispatch : unit =
    let start = TimeHelpers.getTimeMs ()
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    printf "length: %A" (List.length selectedWaves)
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}
    |> TimeHelpers.instrumentInterval "toggleSelectAll" start

/// Row in wave selection table that selects all values in wsModel.AllWaves
let selectAll (wsModel: WaveSimModel) dispatch =
    let allWavesSelected = Map.forall (fun index _ -> isWaveSelected wsModel index) wsModel.AllWaves

    tr summaryProps [
        th [] [
            Checkbox.checkbox []
                [ Checkbox.input [
                    Props 
                        (checkboxInputProps @ [
                            Checked allWavesSelected
                            OnChange(fun _ -> toggleSelectAll (not allWavesSelected) wsModel dispatch )
                    ])
                ] ]
            ]
        th [] [str "Select All"]
    ]

/// Toggle selection for a single wave.
let toggleWaveSelection (index: WaveIndexT) (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    let selectedWaves =
        if List.contains index wsModel.SelectedWaves then
            List.except [index] wsModel.SelectedWaves
        else [index] @ wsModel.SelectedWaves
    let wsModel = {wsModel with SelectedWaves = selectedWaves}
    dispatch <| InitiateWaveSimulation wsModel

/// Toggle selection of a list of waves.
let toggleSelectSubGroup (wsModel: WaveSimModel) dispatch (selected: bool) (waves: WaveIndexT list) =
    let selectedWaves =
        if selected then
            List.append wsModel.SelectedWaves waves
        else
            List.except waves wsModel.SelectedWaves
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}

/// Table row of a checkbox and name of a wave.
let checkboxRow (wsModel: WaveSimModel) dispatch (index: WaveIndexT) =
    let fontStyle = if isWaveSelected wsModel index then boldFontStyle else normalFontStyle
    tr  [ fontStyle ]
        [
            td  [ noBorderStyle ]
                [ Checkbox.checkbox []
                    [ Checkbox.input [
                        Props (checkboxInputProps @ [
                            OnChange(fun _ -> toggleWaveSelection index wsModel dispatch )
                            Checked <| isWaveSelected wsModel index
                        ])
                    ] ]
                ]
            td  [ noBorderStyle ]
                [ str wsModel.AllWaves[index].DisplayName ]
        ]

/// Group of rows for a subgroup of waves. Grouped by ComponentGroups. Waves are hidden in a details element.
let labelRows (compGroup: ComponentGroup) (waves: Wave list) (wsModel: WaveSimModel) dispatch : ReactElement =
    let indices = List.map (fun x -> x.WaveId) waves
    let subGroupSelected = List.forall (fun index -> isWaveSelected wsModel index) indices

    tr summaryProps [
        th [] [
            Checkbox.checkbox [] [
                Checkbox.input [
                    Props [
                        Checked subGroupSelected
                        OnChange (fun _ -> toggleSelectSubGroup wsModel dispatch (not subGroupSelected) indices)
                    ]
                ]
            ]
        ]
        th [] [
            details
                detailsProps
                [   summary
                        summaryProps
                        [ summaryName compGroup ]
                    Table.table [] [
                        tbody []
                            (List.map (checkboxRow wsModel dispatch) indices)
                    ]
                ]
        ]
    ]

/// Search bar to allow users to filter out waves by DisplayName
let searchBar (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    Input.text [
        Input.Option.Props [
            Style [
                MarginBottom "1rem"
            ]
        ]
        Input.Option.Placeholder "Search"
        Input.Option.OnChange (fun c ->
            dispatch <| SetWSModel {wsModel with SearchString = c.Value.ToUpper()}
        )
    ]

/// Table of selectable waves. Waves are grouped by their component type.
let selectWaves (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let selectionRows : ReactElement list =
        Map.values wsModel.AllWaves |> Seq.toList
        |> List.filter (fun x -> x.DisplayName.ToUpper().Contains(wsModel.SearchString))
        |> List.sortBy (fun wave -> wave.DisplayName)
        |> List.groupBy (fun wave ->
            match wave.Type with
            | Input1 _ | Output _ | Constant1 _ ->
                InputOutput
            | IOLabel ->
                WireLabel
            | Viewer _ ->
                Viewers
            | Not | And | Or | Xor | Nand | Nor | Xnor ->
                Gates
            | BusCompare _ ->
                Buses
            | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | Decode4
                // MuxDemux
            | NbitsAdder _ | NbitsXor _ | NbitsAnd _ | NbitsNot _ | NbitSpreader _
                // Arithmetic
            | Custom _
                // CustomComp
            | DFF | DFFE | Register _ | RegisterE _
                // FFRegister
            | AsyncROM1 _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ ->
                // Memories
                Component wave.CompLabel
            | BusSelection _ | MergeWires | SplitWire _ ->
                failwithf "Bus select, MergeWires, SplitWire should not appear"
            | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
                failwithf "Legacy component types should not appear"
        )
        |> List.map (fun (compGroup, waves) ->
            labelRows compGroup waves wsModel dispatch
        )

    Table.table [
        Table.IsBordered
        Table.IsFullWidth
        Table.Props [
            Style [BorderWidth 0]
        ]
    ] [ thead []
            ( [selectAll wsModel dispatch] @
                selectionRows
            )
    ]

/// Button to activate wave selection modal
let selectWavesButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let waveCount = Map.count wsModel.AllWaves
    let props, buttonFunc =
        if waveCount > 0 then
            selectWavesButtonProps, (fun _ -> dispatch <| SetWSModel {wsModel with WaveModalActive = true})
        else selectWavesButtonPropsLight, (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select Waves")

/// Modal that, when active, allows users to select waves to be viewed.
let selectWavesModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    Modal.modal [
        Modal.IsActive wsModel.WaveModalActive
    ] [
        Modal.background [
            Props [
                OnClick (fun _ -> dispatch <| SetWSModel {wsModel with WaveModalActive = false})
            ]
        ] []
        Modal.Card.card [] [
            Modal.Card.head [] [
                Modal.Card.title [] [
                    Level.level [] [
                        Level.left [] [ str "Select Waves" ]
                        Level.right [
                        ] [ Delete.delete [
                                Delete.Option.Size IsMedium
                                Delete.Option.OnClick (fun _ ->
                                    dispatch <| SetWSModel
                                        {wsModel with
                                            WaveModalActive = false
                                            SearchString = ""
                                        }
                                )
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [] [
                searchBar wsModel dispatch
                selectWaves wsModel dispatch
            ]
            Modal.Card.foot [] []
        ]
    ]

/// Button to activate RAM selection modal.
let selectRamButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramCount = List.length wsModel.RamComps
    let props, buttonFunc =
        if ramCount > 0 then
            selectRamButtonProps, (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = true})
        else selectRamButtonPropsLight, (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select RAM")

/// Toggle if a RAM's contents is selected for viewing.
let toggleRamSelection (ramId: FComponentId) (ramLabel: string) (wsModel: WaveSimModel) dispatch =
    let selectedRams =
        if isRamSelected ramId wsModel then
            Map.remove ramId wsModel.SelectedRams
        else
            Map.add ramId ramLabel wsModel.SelectedRams
    dispatch <| SetWSModel {wsModel with SelectedRams = selectedRams}

/// Modal that, when active, allows users to select RAMs to view their contents.
let selectRamModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramRows (ram: FastComponent) : ReactElement =
        tr [] [
            td []
                [ Checkbox.checkbox []
                    [ Checkbox.input [
                        Props (checkboxInputProps @ [
                            Checked <| isRamSelected ram.fId wsModel
                            OnChange (fun _ -> toggleRamSelection ram.fId ram.FullName wsModel dispatch)
                        ])
                    ] ]
                ]
            td [] [ label [ ramRowStyle ] [ str ram.FullName ] ]
        ]

    Modal.modal [
        Modal.IsActive wsModel.RamModalActive
    ] [
        Modal.background [
            Props [
                OnClick (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = false})
            ]
        ] []
        Modal.Card.card [] [
            Modal.Card.head [] [
                Modal.Card.title [] [
                    Level.level [] [
                        Level.left [] [ str "Select RAM" ]
                        Level.right [] [
                            Delete.delete [
                                Delete.Option.Size IsMedium
                                Delete.Option.OnClick (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = false})
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [] [
                str "Select synchronous RAM components to view their contents. "
                str "Note that asynchronous RAM components cannot be viewed in the waveform simulator. "
                br []
                br []
                str "On a write, the corresponding row will be highlighted in red. "
                str "On a read, the corresponding row will be highlighted in blue. "
                str "Any memory address which has not been initialised with a value will not be shown in the table. "
                hr []
                Table.table [] [
                    tbody []
                        (List.map (ramRows) wsModel.RamComps)
                ]
            ]

            Modal.Card.foot [] []
        ]
    ]

/// Set highlighted clock cycle number
let private setClkCycle (wsModel: WaveSimModel) (dispatch: Msg -> unit) (newClkCycle: int) : unit =
    let start = TimeHelpers.getTimeMs ()
    let newClkCycle = min Constants.maxLastClk newClkCycle |> max 0

    if newClkCycle <= endCycle wsModel then
        if newClkCycle < wsModel.StartCycle then
            dispatch <| InitiateWaveSimulation
                {wsModel with 
                    StartCycle = newClkCycle
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
        else
            dispatch <| SetWSModel
                {wsModel with
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
    else
        dispatch <| InitiateWaveSimulation
            {wsModel with
                StartCycle = newClkCycle - (wsModel.ShownCycles - 1)
                CurrClkCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
            }
    |> TimeHelpers.instrumentInterval "setClkCycle" start

/// if zoomIn, then increase width of clock cycles (i.e.reduce number of visible cycles)
let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) =
    let start = TimeHelpers.getTimeMs ()
    let shownCycles =
        if zoomIn then
            let newCycles = int <| float wsModel.ShownCycles * 0.8

            // If number of cycles after casting to int does not change
            if newCycles = int wsModel.ShownCycles then
                wsModel.ShownCycles - 1
            // Require at least one visible cycle
            else max 1 (newCycles)
        else
            let newCycles = int <| float wsModel.ShownCycles * 1.25

            // If number of cycles after casting to int does not change
            if newCycles = wsModel.ShownCycles then
                wsModel.ShownCycles + 1
            // If width of clock cycle is too small
            else if wsModel.WaveformColumnWidth / float newCycles < Constants.minCycleWidth then
                wsModel.ShownCycles
            else newCycles

    dispatch <| InitiateWaveSimulation { wsModel with ShownCycles = shownCycles }
    |> TimeHelpers.instrumentInterval "changeZoom" start

/// Click on these buttons to change the number of visible clock cycles.
let zoomButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ clkCycleButtonStyle ]
        [
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> changeZoom wsModel false dispatch)
                zoomOutSVG
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> changeZoom wsModel true dispatch)
                zoomInSVG
        ]

/// Click on these to change the highlighted clock cycle.
let clkCycleButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    /// Controls the number of cycles moved by the "◀◀" and "▶▶" buttons
    let bigStepSize = max 1 (wsModel.ShownCycles / 2)

    let scrollWaveformsBy (numCycles: int) =
        setClkCycle wsModel dispatch (wsModel.CurrClkCycle + numCycles)

    div [ clkCycleButtonStyle ]
        [
            // Move left by bigStepSize cycles
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> scrollWaveformsBy -bigStepSize)
                (str "◀◀")

            // Move left by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy -1)
                (str "◀")

            // Text input box for manual selection of clock cycle
            Input.number [
                Input.Props clkCycleInputProps

                Input.Value (
                    match wsModel.ClkCycleBoxIsEmpty with
                    | true -> ""
                    | false -> string wsModel.CurrClkCycle
                )
                // TODO: Test more properly with invalid inputs (including negative numbers)
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n ->
                        setClkCycle wsModel dispatch n
                    | false, _ when c.Value = "" ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = true}
                    | _ ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = false}
                )
            ]

            // Move right by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy 1)
                (str "▶")

            // Move right by bigStepSize cycles
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> scrollWaveformsBy bigStepSize)
                (str "▶▶")
        ]

/// ReactElement of the tabs for changing displayed radix
let private radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let radixString = [
        Bin,  "Bin"
        Hex,  "Hex"
        Dec,  "uDec"
        SDec, "sDec"
    ]

    let radixTab (radix, radixStr) =
        Tabs.tab [
            Tabs.Tab.IsActive(wsModel.Radix = radix)
            Tabs.Tab.Props radixTabProps
        ] [ a [
            radixTabAStyle
            OnClick(fun _ -> dispatch <| InitiateWaveSimulation {wsModel with Radix = radix})
            ] [ str radixStr ]
        ]

    Tabs.tabs [
        Tabs.IsToggle
        Tabs.Props [ radixTabsStyle ]
    ] (List.map (radixTab) radixString)

/// Create label of waveform name for each selected wave.
/// Note that this is generated after calling selectedWaves. Any changes to this function
/// must also be made to valueRows and waveRows, as the order of the waves matters here.
/// This is because the wave viewer is comprised of three columns of many rows, rather
/// than many rows of three columns.
let nameRows (model: Model) (wsModel: WaveSimModel) dispatch: ReactElement list =
    selectedWaves wsModel
    |> List.map (fun wave ->
        let visibility =
            if wsModel.HoveredLabel = Some wave.WaveId then
                "visible"
            else "hidden"

        Level.level [
            Level.Level.Option.Props [
                nameRowLevelStyle (wsModel.HoveredLabel = Some wave.WaveId)

                OnMouseOver (fun _ ->
                    if wsModel.DraggedIndex = None then
                        dispatch <| SetWSModel {wsModel with HoveredLabel = Some wave.WaveId}
                        // Check if symbol exists on Canvas
                        if Map.containsKey wave.WaveId.Id model.Sheet.Wire.Symbol.Symbols then
                            dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols [wave.WaveId.Id])))
                        // Filter out any non-existent wires
                        let conns = List.filter (fun conn -> Map.containsKey conn model.Sheet.Wire.Wires) wave.Conns 
                        dispatch <| Sheet (SheetT.Msg.SelectWires conns)
                )
                OnMouseOut (fun _ ->
                    dispatch <| SetWSModel {wsModel with HoveredLabel = None}
                    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols [])))
                    dispatch <| Sheet (SheetT.Msg.UpdateSelectedWires (wave.Conns, false))
                )

                Draggable true

                OnDragStart (fun ev ->
                    ev.dataTransfer.effectAllowed <- "move"
                    ev.dataTransfer.dropEffect <- "move"
                    dispatch <| SetWSModel {
                        wsModel with
                            DraggedIndex = Some wave.WaveId
                            PrevSelectedWaves = Some wsModel.SelectedWaves
                        }
                )

                OnDrag (fun ev -> 
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()

                    // If the user drags the label outside the bounds of the wave name column
                    if ev.clientX < bcr.left || ev.clientX > bcr.right ||
                        ev.clientY < bcr.top || ev.clientY > bcr.bottom
                    then
                        dispatch <| SetWSModel {
                            wsModel with
                                HoveredLabel = Some wave.WaveId
                                // Use wsModel.SelectedValues if somehow PrevSelectedWaves not set
                                SelectedWaves = Option.defaultValue wsModel.SelectedWaves wsModel.PrevSelectedWaves
                            }
                )

                OnDragOver (fun ev -> ev.preventDefault ())

                OnDragEnter (fun ev ->
                    ev.preventDefault ()
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()
                    let index = int (ev.clientY - bcr.top) / Constants.rowHeight - 1
                    let draggedWave =
                        match wsModel.DraggedIndex with
                        | Some waveId -> [waveId]
                        | None -> []

                    let selectedWaves =
                        wsModel.SelectedWaves
                        |> List.except draggedWave
                        |> List.insertManyAt index draggedWave

                    dispatch <| SetWSModel {wsModel with SelectedWaves = selectedWaves}
                )

                OnDragEnd (fun _ ->
                    dispatch <| SetWSModel {
                        wsModel with
                            DraggedIndex = None
                            PrevSelectedWaves = None
                        }
                )
            ]
        ] [ Level.left
                [ Props (nameRowLevelLeftProps visibility) ]
                [ Delete.delete [
                    Delete.Option.Size IsSmall
                    Delete.Option.Props [
                        OnClick (fun _ ->
                            let selectedWaves = List.except [wave.WaveId] wsModel.SelectedWaves
                            dispatch <| SetWSModel {wsModel with SelectedWaves = selectedWaves}
                        )
                    ]
                  ] []
                ]
            Level.right
                [ Props [ Style [ PaddingRight Constants.labelPadding ] ] ]
                [ label [ nameLabelStyle (wsModel.HoveredLabel = Some wave.WaveId) ] [ wave.DisplayName |> WaveSimHelpers.camelCaseDottedWords |> str ] ]
        ]
    )

/// Create column of waveform names
let namesColumn model wsModel dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let rows = 
        nameRows model wsModel dispatch
    div (namesColumnProps wsModel)
        (List.concat [ topRow; rows ])
    |> TimeHelpers.instrumentInterval "namesColumn" start


/// Create label of waveform value for each selected wave at a given clk cycle.
/// Note that this is generated after calling selectedWaves.
/// Any changes to this function must also be made to nameRows
/// and waveRows, as the order of the waves matters here. This is
/// because the wave viewer is comprised of three columns of many
/// rows, rather than many rows of three columns.
let valueRows (wsModel: WaveSimModel) =
    selectedWaves wsModel
    |> List.map (getWaveValue wsModel.CurrClkCycle)
    |> List.map (valToString wsModel.Radix)
    |> List.map (fun value -> label [ valueLabelStyle ] [ str value ])

/// Create column of waveform values
let private valuesColumn wsModel : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let rows = valueRows wsModel

    div [ valuesColumnStyle ]
        (List.concat [ topRow; rows ])
    |> TimeHelpers.instrumentInterval "valuesColumn" start

/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles.
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        match (singleWaveWidth wsModel) with
        | width when width < Constants.clkCycleNarrowThreshold && i % 5 <> 0 -> []
        | _ -> [ text (clkCycleText wsModel i) [str (string i)] ]

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> svg (clkCycleNumberRowProps wsModel)

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (wsModel: WaveSimModel) dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    /// Note that this is generated after calling selectedWaves.
    /// Any changes to this function must also be made to nameRows
    /// and valueRows, as the order of the waves matters here. This is
    /// because the wave viewer is comprised of three columns of many
    /// rows, rather than many rows of three columns.
    let waveRows : ReactElement list =
        selectedWaves wsModel
        |> List.map (fun wave ->
            match wave.SVG with
            | Some waveform ->
                waveform
            | None ->
                printf "no waveform generated for %A" wave.DisplayName
                div [] []
        )

    div [ waveformColumnStyle ]
        [
            clkCycleHighlightSVG wsModel dispatch
            div [ waveRowsStyle wsModel.WaveformColumnWidth]
                ([ clkCycleNumberRow wsModel ] @
                    waveRows
                )
        ]
    |> TimeHelpers.instrumentInterval "waveformColumn" start

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (model: Model) (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ showWaveformsStyle ]
        [
            namesColumn model wsModel dispatch
            waveformColumn wsModel dispatch
            valuesColumn wsModel
        ]

/// Table row that shows the address and data of a RAM component.
let ramTableRow ((addr, data,rowType): string * string * RamRowType): ReactElement =

    tr [ Style <| ramTableRowStyle rowType ] [
        td [] [ str addr ]
        td [] [ str data ]
    ]

/// Table showing contents of a RAM component.
let ramTable (wsModel: WaveSimModel) ((ramId, ramLabel): FComponentId * string) : ReactElement =

    let fs = wsModel.FastSim
    let fc = wsModel.FastSim.FComps[ramId]
    let step = wsModel.CurrClkCycle
    let memData =
        match fc.FType with
        | ROM1 mem
        | AsyncROM1 mem -> mem
        | RAM1 mem
        | AsyncRAM1 mem -> 
            match FastRun.extractFastSimulationState fs wsModel.CurrClkCycle ramId with
            |RamState mem -> mem
            | _ -> failwithf $"What? Can't find state from RAM component '{ramLabel}'"
        | _ -> failwithf $"Given a component {fc.FType} which is not a vaild RAM"
    let aWidth,dWidth = memData.AddressWidth,memData.WordWidth

    let print w (a:int64) = NumberHelpers.valToPaddedString w wsModel.Radix (((1L <<< w) - 1L) &&& a)

    let lastLocation = int64 ((2 <<< memData.AddressWidth - 1) - 1)

    /// print a single 0 location as one table row
    let print1 (a:int64,b:int64,rw:RamRowType) = $"{print aWidth a}",$"{print dWidth b}",rw
    /// print a range of zero locations as one table row

    let print2 (a1:int64) (a2:int64) (d:int64) = $"{print aWidth (a1+1L)}..{print aWidth (a2-1L)}", $"{print dWidth d}",RAMNormal

    /// output info for one table row filling the given zero memory gap or arbitrary size, or no line if there is no gap.
    let printGap (gStart:int64) (gEnd:int64) =
        match gEnd - gStart with
        | 1L -> []
        | 2L -> [print1 ((gEnd + gStart) / 2L, 0L,RAMNormal)]
        | n when n > 2L ->
            [print2 gStart gEnd 0L]
        | _ ->
            failwithf $"What? gEnd={gEnd},gStart={gStart}: negative or zero gaps are impossible..."

    /// transform Sparse RAM info into strings to print in a table, adding extra lines for zero gaps
    /// line styling is controlled by a RamRowtype value and added later when the table row react is generated
    let addGapLines (items: (int64*int64*RamRowType) list) = 
        let startItem =
            match items[0] with
            | -1L,_,_ -> []
            | gStart,dStart,rw-> [print1 (gStart,dStart,rw)]
        List.pairwise items
        |> List.collect (fun ((gStart,_,_),(gEnd,dEnd,rwe)) -> 
            let thisItem = if gEnd = lastLocation + 1L then [] else [print1 (gEnd,dEnd,rwe)]
            [printGap gStart gEnd; thisItem])
        |> List.concat

    /// Add a RAMNormal RamRowType value to every location in mem.
    /// Add in additional locations for read and/or write if needed.
    /// Set RamRowValue type to RAMWritten or RAMRead for thse locations.
    /// Write is always 1 cycle after WEN=1 and address.
    /// Read is 1 (0) cycles after address for sync (asynch) memories.
    let addReadWrite (fc:FastComponent) (step:int) (mem: Map<int64,int64>) =
        let getFData (fd: FData) =
            match fd with
            | Data {Dat= Word w} -> int64 w
            | Data {Dat=BigWord bw} -> int64 bw
            | _ -> 
                printfn $"Help! Can'd find data from {fd}"
                int64 <| -1

        let readStep =
            match fc.FType with
            | AsyncROM1 _ | AsyncRAM1 _ -> step
            | ROM1 _ | RAM1 _ -> step - 1
            | _ -> failwithf $"What? {fc.FullName} should be a memory component"

        let addrSteps step = fc.InputLinks[0].Step[step]

        let readOpt =
            match step, fc.FType with
            | 0,ROM1 _ | 0, RAM1 _ -> None
            | _ -> 
                addrSteps readStep
                |> getFData
                |> Some
        let writeOpt =
            match step, fc.FType with
            | _, ROM1 _ 
            | _, AsyncROM1 _
            | 0, _ -> None
            | _, RAM1 _ | _, AsyncRAM1 _ when getFData fc.InputLinks[2].Step[step-1] = 1L -> 
                addrSteps (step-1)
                |> Some
            | _ ->  
                None
            |> Option.map getFData

        /// Mark addr in memory map as being rType
        /// if addr does not exist - create it
        let addToMap rType addr mem:Map<int64,int64*RamRowType> =
            match Map.tryFind addr mem with
            | Some (d,_) -> Map.add addr (d,rType) mem
            | None  ->  Map.add addr (0L,rType) mem
    

        Map.map (fun k v -> v,RAMNormal) mem
        |> (fun mem ->
            match readOpt with
            | Some addr -> addToMap RAMRead addr mem
            | None -> mem
            |> (fun mem ->
                match writeOpt with // overwrite RAMRead here is need be
                | Some addr -> addToMap RAMWritten addr mem
                | None -> mem))
 

    /// add fake locations beyong normal address range so that
    /// addGapLines fills these (if need be). These locations are then removed
    let addEndPoints (items:(int64*int64*RamRowType) list)  =
        let ad (a,d,rw) = a
        match items.Length with
        | 0 -> [-1L,0L,RAMNormal;  lastLocation,0L,RAMNormal]
        | _ ->
            if ad items[0] < 0L then items else List.insertAt 0 (-1L,-1L,RAMNormal) items
            |> (fun items ->
                if ad items[items.Length-1] = lastLocation then 
                    items else 
                List.insertAt items.Length (lastLocation+1L,0L,RAMNormal) items)
    

    let lineItems =
        memData.Data
        |> addReadWrite fc step
        |> Map.toList
        |> List.map (fun (a,(d,rw)) -> a,d,rw)
        |> List.filter (fun (a,d,rw) -> d<>0L || rw <> RAMNormal)
        |> List.sort
        |> addEndPoints 
        |> addGapLines
        


    Level.item [
        Level.Item.Option.Props ramTableLevelProps
        Level.Item.Option.HasTextCentered
    ] [
        Heading.h6 [
            Heading.Option.Props [ centerAlignStyle ]
        ] [ str ramLabel ]
        div [Style [MaxHeight "600px";OverflowY OverflowOptions.Auto]] [
        Table.table [
            Table.IsFullWidth
            Table.IsBordered
        ] [ thead [] [
                tr [] [
                    th [ centerAlignStyle ] [ str "Address"]
                    th [ centerAlignStyle ] [ str "Data"]
                ]
            ]
            tbody []
                (List.map ramTableRow lineItems) 
        ] ]
        br []
    ]

/// Bulma Level component of tables showing RAM contents.
let ramTables (wsModel: WaveSimModel) : ReactElement =
    let inlineStyle (styles:CSSProp list) = div [Style (Display DisplayOptions.Inline :: styles)]
    let start = TimeHelpers.getTimeMs ()
    let selectedRams = Map.toList wsModel.SelectedRams
    if List.length selectedRams > 0 then
        let headerRow =
            ["read", RAMRead; "written",RAMWritten]
            |> List.map (fun (op, opStyle) -> inlineStyle [] [str "Memory location "; inlineStyle (ramTableRowStyle  opStyle) [str op]])
            |> function | [a;b] -> [str "Key: " ; a; str ", " ;b; str " In current cycle."] | _ -> failwithf "What? Can't happen!"
        List.map (fun ram -> td [Style [BorderColor "white"]] [ramTable wsModel ram])  selectedRams
        |> (fun tables -> [tbody [] [tr [] [th [ColSpan selectedRams.Length] [inlineStyle [] headerRow]]; tr [Style [Border "10px"]] tables]])
        |> Fulma.Table.table [Table.TableOption.Props ramTablesLevelProps; Table.IsFullWidth; Table.IsBordered; Table.Props [Style [Height "100%"]]]
    else div [] []
    |> TimeHelpers.instrumentInterval "ramTables" start

/// Async function which runs the fast simulator when refreshing the wave sim. Set as asynchronous
/// since the fast simulator takes some time to run the first time it is started.
let refreshWaveSim (wsModel, simData, (comps, conns)) : Async<WaveSimModel> = async {
    let start = TimeHelpers.getTimeMs ()
    FastRun.runFastSimulation Constants.maxLastClk simData.FastSim
    |> TimeHelpers.instrumentInterval "runFastSimulation" start
    let fs = simData.FastSim

    let allWaves =
        let allWavesStart = TimeHelpers.getTimeMs ()
        getWaves simData (comps, conns)
        |> Map.map (generateWaveform wsModel)
        |> TimeHelpers.instrumentInterval "allWaves" allWavesStart

    let ramComps =
        let isRAMOrROM fcid (fc: FastComponent) =
            match fc.FType with
            | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ ->
                true
            | _ -> false
        Map.filter isRAMOrROM simData.FastSim.FComps
        |> Map.toList
        |> List.map (fun (fcid,fc) -> fc)
        |> List.sortBy (fun fc -> fc.FullName)

    let ramCompIds = List.map (fun (fc: FastComponent) -> fc.fId) ramComps

    let selectedWaves = List.filter (fun key -> Map.containsKey key allWaves) wsModel.SelectedWaves
    let selectedRams = Map.filter (fun ramfId _ -> List.contains ramfId ramCompIds) wsModel.SelectedRams

    return {
        wsModel with
            State = Success
            AllWaves = allWaves
            SelectedWaves = selectedWaves
            RamComps = ramComps
            SelectedRams = selectedRams
            FastSim = simData.FastSim
    }
    |> TimeHelpers.instrumentInterval "refreshWaveSim" start
}

/// Refresh the state of the wave simulator according to the model and canvas state.
let refreshButtonAction model dispatch = fun _ ->
    let wsSheet = Option.get (getCurrFile model)
    let wsModel = getWSModel model
    match SimulationView.makeSimData model with
    | None ->
        dispatch <| SetWSModel { wsModel with State = NoProject }
    | Some (Error e, _) ->
        dispatch <| SetWSModelAndSheet ({ wsModel with State = SimError e }, wsSheet)
    | Some (Ok simData, canvState) ->
        if simData.IsSynchronous then
            let wsModel = { wsModel with State = Loading }
            dispatch <| SetWSModelAndSheet (wsModel, wsSheet)
            dispatch <| RefreshWaveSim (wsModel, simData, canvState)

        else
            dispatch <| SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet)

/// ReactElement showing instructions and wave sim buttons
let topHalf (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model
    let loading =
        match wsModel.State with
        | Loading -> true
        | _ -> false
    let refreshButtonSvg = if loading then emptyRefreshSVG else refreshSvg

    div [ topHalfStyle ] [
        br []
        Level.level [] [
            Level.left [] [
                Heading.h4 [] [ str "Waveform Simulator" ]
            ]
            Level.right [] [
                button
                    [ Button.Option.IsLoading loading ]
                    (refreshButtonAction model dispatch)
                    refreshButtonSvg
            ]
        ]

        Columns.columns [] [
            Column.column [] [
                str "View sequential logic using the waveform simulator by selecting desired waveforms. "
                str "Select synchronous RAM components to view their contents during the simulation. "
                str "You must restart the waveform simulator to view any changes to the circuit. "
            ]

            Column.column [
                Column.Option.Width (Screen.All, Column.IsNarrow)
            ] [ Level.level [] [
                    Level.item [ ] [
                        Button.list [] [
                            selectWavesButton wsModel dispatch
                            selectWavesModal wsModel dispatch

                            selectRamButton wsModel dispatch
                            selectRamModal wsModel dispatch
                        ]
                    ]
                ]
                Level.level [] [
                    Level.left [] [
                        zoomButtons wsModel dispatch
                    ]
                    Level.right [] [
                        radixButtons wsModel dispatch
                    ]
                ]
                clkCycleButtons wsModel dispatch
            ]
        ]
        hr [ Style [ MarginBottom "5px" ] ]
        br []
    ]

/// Entry point to the waveform simulator.
let viewWaveSim (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model
    div [ viewWaveSimStyle ]
        [
            topHalf model dispatch

            match wsModel.State with
            | Empty ->
                div [ errorMessageStyle ]
                    [ str "Start the waveform simulator by pressing the refresh button." ]
            | NoProject ->
                div [ errorMessageStyle ]
                    [ str "Please open a project to use the waveform simulator." ]
            | SimError e ->
                div [ errorMessageStyle ]
                    [ SimulationView.viewSimulationError e ]
            | NonSequential ->
                div [ errorMessageStyle ]
                    [ str "There is no sequential logic in this circuit." ]
            | Loading | Success ->
                div [showWaveformsAndRamStyle] [

                    showWaveforms model wsModel dispatch

                    hr []

                    ramTables wsModel
                    ]

            hr []
        ]
