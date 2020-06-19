(*
    CanvasStateAnalyser.fs

    This module collects a series of functions that perform checks on
    CanvasState and SimulationGraph.
*)

module CanvasStateAnalyser

open CommonTypes
open SimulatorTypes
open BusWidthInferer

// -- Checks performed
//
// Ports constraints:
// - Source ports must be output ports.
// - Target ports must be input ports.
// - All ports have at least one connection that touches them.
// - Input ports have precisely one connection that touches them.
//
// Input/Output components in a simulationgraph must all have unique labels.
//
// All wire widths are consistent (rely on WidthInferer.fs).

/// Check that:
/// 1- all source ports in connections are Output ports,
/// 2- all target ports in connections are Input ports,
/// 3- all input ports in a component are actually input ports,
/// 4- all output ports in a component are actually output ports,
/// 5- all ports on components have a port number,
/// 6- all ports on connection do not have a port number.
/// These conditions should always hold, unless there are bugs in the code (i.e.
/// no user behaviour should be able to trigger such errors).
/// The costruction of the Simulation graph assumes that these rules hold.
/// TODO: should they crash the program then?
let private checkPortTypesAreConsistent (canvasState : CanvasState) : SimulationError option =
    let rec checkComponentPorts (ports : Port list) (correctType : PortType) =
        match ports with
        | [] -> None
        | port :: _ when port.PortNumber = None -> Some {
            Msg = sprintf "%A port appears to not have a port number" correctType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | port :: _ when port.PortType <> correctType -> Some {
            Msg = sprintf "%A port %d appears to be an %A port" correctType (Option.get port.PortNumber) port.PortType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | _ :: ports' ->
            checkComponentPorts ports' correctType
    /// Check conditions 3, 4, 5
    let rec checkComponentsPorts (components : Component list) =
        match components with
        | [] -> None
        | comp :: components' ->
            match checkComponentPorts comp.InputPorts PortType.Input,
                  checkComponentPorts comp.OutputPorts PortType.Output with
            | Some err, _ | _, Some err -> Some err
            | None, None -> checkComponentsPorts components' // Check next.

    let checkConnectionPort (port : Port) (correctType : PortType) (connId : string) =
        match port.PortType = correctType, port.PortNumber with
        | false, _ -> Some {
            Msg = sprintf "%A port appears to be an %A port" correctType port.PortType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | _, Some pNumber -> Some {
            Msg = sprintf "%A port appears to have a port number: %d" correctType pNumber
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | true, None -> None // All right.
    /// Check conditions 1, 2, 6
    let rec checkConnectionsPorts (connections : Connection list) =
        match connections with
        | [] -> None
        | conn :: connections' ->
            match checkConnectionPort conn.Source PortType.Output conn.Id,
                  checkConnectionPort conn.Target PortType.Input conn.Id with
            | Some err, _ | _, Some err -> Some err
            | None, None -> checkConnectionsPorts connections' // Check next.

    let components, connections = canvasState
    match checkComponentsPorts components,
          checkConnectionsPorts connections with
    | Some err, _ | _, Some err -> Some err
    | None, None -> None // All right.

/// Return all the Ids of all input ports across all components.
/// Return also the ComponentId which may be used in error messages.
let private getAllInputPortIds (components : Component list) : (InputPortId * ComponentId) list =
    components |> List.collect
        (fun comp -> comp.InputPorts |> List.map (fun port -> InputPortId port.Id, ComponentId comp.Id))

/// Return all the Ids of all ouput ports across all components.
/// Return also the ComponentId which may be used in error messages.
let private getAllOutputPortIds (components : Component list) : (OutputPortId * ComponentId) list =
    components |> List.collect
        (fun comp -> comp.OutputPorts |> List.map (fun port -> OutputPortId port.Id, ComponentId comp.Id))

/// Count the number of connections that target each port.
let rec private countPortsConnections
        (connections : Connection list)
        (inputCounts : Map<InputPortId * ComponentId, int>)
        (outputCounts : Map<OutputPortId * ComponentId, int>)
        : Result< Map<InputPortId * ComponentId, int> * Map<OutputPortId * ComponentId, int>, SimulationError> =
    match connections with
    | [] -> Ok (inputCounts, outputCounts)
    | conn :: connections' ->
        let sourceId = OutputPortId conn.Source.Id
        let targetId = InputPortId conn.Target.Id
        let sourceHostId = ComponentId conn.Source.HostId
        let targetHostId = ComponentId conn.Target.HostId
        let outputCountsRes =
            match outputCounts.TryFind (sourceId, sourceHostId) with
            | None -> failwithf "Connection refers to a source port that does not exist: %s" conn.Source.Id
            | Some count -> Ok <| outputCounts.Add ((sourceId, sourceHostId), count + 1)
        let inputCountsRes =
            match inputCounts.TryFind (targetId, targetHostId) with
            | None -> failwithf "what? Connection refers to a target port that does not exist: %s" conn.Target.Id
            | Some count -> Ok <| inputCounts.Add ((targetId, targetHostId), count + 1)
        match inputCountsRes, outputCountsRes with
        | Error err, _ | _, Error err -> Error err
        | Ok inputCounts, Ok outputCounts ->
            countPortsConnections connections' inputCounts outputCounts

/// Apply condition on evaery element of the map (tailored to this specific
/// problem).
let private checkEvery
        (counts : Map<'a * ComponentId, int>) // 'a is either InputPortId or OutputPortId.
        (cond : int -> bool)
        errMsg
        : SimulationError option =
    (None, counts) ||> Map.fold (fun maybeErr (_, componentId) count ->
        match maybeErr with
        | Some err -> Some err
        | None ->
            // Return special error message if there are zero connections.
            match cond count with
            | true -> None
            | false when count = 0 -> Some {
                Msg = "All ports must have at least one connection."
                InDependency = None
                ComponentsAffected = [componentId]
                ConnectionsAffected = [] }
            | false -> Some {
                Msg = sprintf errMsg count 
                InDependency = None
                ComponentsAffected = [componentId]
                ConnectionsAffected = [] }
    )

/// Check that:
/// - any port has at least one connection,
/// - any input port has precisely one connection.
/// These conditions may not hold due to user errors.
let private checkPortsAreConnectedProperly
        (canvasState : CanvasState)
        : SimulationError option =
    let components, connections = canvasState
    let inputCounts =
        getAllInputPortIds components
        |> List.map (fun portId -> portId, 0)
        |> Map.ofList
    let outputCounts =
        getAllOutputPortIds components
        |> List.map (fun portId -> portId, 0)
        |> Map.ofList
    match countPortsConnections connections inputCounts outputCounts with
    | Error err -> Some <| err
    | Ok (inputCounts, outputCounts) ->
        let inputRes = checkEvery inputCounts ((=) 1) "A wire must have precisely one driving component, but %d were found. If you want to merge wires together, use a MergeWires component."
        let outputRes = checkEvery outputCounts ((<=) 1) "Output port receives an unexpected number of connections: %d." // This error message should not be triggered.
        match inputRes, outputRes with
        | None, None -> None
        | Some err, _ | _, Some err -> Some err

/// Input/Output components in a simulationgraph all have unique labels.
let private checkIOLabels (canvasState : CanvasState) : SimulationError option =
    let rec checkDuplicate (comps : Component list) (map : Map<string,string>) (ioType : string) =
        match comps with
        | [] -> None
        | comp :: comps' ->
            match map.TryFind comp.Label with
            | None -> checkDuplicate comps' map ioType
            | Some compId when compId = comp.Id -> checkDuplicate comps' map ioType
            | Some compId -> Some {
                Msg = sprintf "Two %s components cannot have the same label: %s." ioType comp.Label
                InDependency = None
                ComponentsAffected = [comp.Id; compId] |> List.map ComponentId
                ConnectionsAffected = []
            }
    let toMap (comps : Component list) =
        comps |> List.map (fun comp -> comp.Label, comp.Id) |> Map.ofList
    let components, _ = canvasState
    let inputs =
        components
        |> List.filter (fun comp -> match comp.Type with | Input _ -> true | _ -> false)
    let outputs =
        components
        |> List.filter (fun comp -> match comp.Type with | Output _ -> true | _ -> false)
    match checkDuplicate inputs (toMap inputs) "Input",
          checkDuplicate outputs (toMap outputs) "Output" with
    | Some err, _ | _, Some err -> Some err
    | None, None -> None

/// Checks that all connections have consistent widths.
/// This function relies on the bus inferer, but also makes sure that all widths
/// can be inferred.
let private checkConnectionsWidths
        (canvasState : CanvasState)
        : SimulationError option =
    let convertConnId (BusTypes.ConnectionId cId) = ConnectionId cId
    let convertError (err : BusTypes.WidthInferError) : SimulationError = {
        Msg = err.Msg
        InDependency = None
        ConnectionsAffected = err.ConnectionsAffected |> List.map convertConnId
        ComponentsAffected = []
    }
    match inferConnectionsWidth canvasState with
    | Error err -> Some <| convertError err
    | Ok connWidths ->
        let faulty = connWidths |> Map.filter (fun _ width -> Option.isNone width)
        match faulty.IsEmpty with
        | true -> None // All good.
        | _ -> Some {
            Msg = "Could not infer all connections widths."
            InDependency = None
            ConnectionsAffected =
                faulty |> Map.toList |> List.map (fun (cId, _) -> convertConnId cId)
            ComponentsAffected = []
        }

/// Analyse a CanvasState and return any error (or None).
let analyseState
        (state : CanvasState)
        : SimulationError option =
    [
        checkPortTypesAreConsistent state
        checkPortsAreConnectedProperly state
        checkIOLabels state
        checkConnectionsWidths state
    ]
    |> List.tryFind Option.isSome
    |> Option.flatten
