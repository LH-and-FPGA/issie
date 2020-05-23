(*
    Runner.fs

    This module collects functions that allow to feed input into the simulation,
    effectively allowing to run it.
*)

module SimulationRunner

open DiagramTypes
open SimulatorTypes

// During simulation, a Component Reducer function will produce the output only
// when all of the expected inputs have a value. Once this happens, it will
// calculate its outputs and set them in the next simulationComponent(s).

/// Take the Input, and feed it to the Component with the specified Id.
/// This function should be used to feed combinational logic inputs, not to
/// trigger a clock tick.
/// If the Component is then ready to produce an output, propagate this output
/// by recursively feeding it as an input to the connected Components.
let rec private feedInput
        (graph : SimulationGraph)
        (compId : ComponentId)
        (input : InputPortNumber * WireData)
        : SimulationGraph =
    // Extract component.
    let comp = match graph.TryFind compId with
               | None -> failwithf "what? Could not find component %A in simulationStep" compId
               | Some c -> c
    // Add input to the simulation component.
    let comp = { comp with Inputs = comp.Inputs.Add input }
    let graph = graph.Add (comp.Id, comp)
    // Prepare reducer Input.
    let reducerInput = {
        Inputs = comp.Inputs
        CustomSimulationGraph = comp.CustomSimulationGraph
        IsClockTick = false
    }
    // Try to reduce the component.
    let reducerOutput = comp.Reducer reducerInput
    // Check wether the reducer produced any outputs.
    match reducerOutput.Outputs with
    | None -> graph // Keep on waiting for more inputs.
    | Some outputMap ->
        // Received enough inputs and produced an output.
        // Propagate each output produced.
        let graph = feedReducerOutput comp graph outputMap
        // Update the CustomSimulationGraph and return the new simulation graph.
        let comp = { comp with CustomSimulationGraph = reducerOutput.NewCustomSimulationGraph }
        graph.Add (comp.Id, comp)

/// Propagate each output produced by a simulation component to all the
/// components connected to its output ports.
/// Return the updated simulationGraph.
and private feedReducerOutput
        (comp : SimulationComponent)
        (graph : SimulationGraph)
        (outputMap : Map<OutputPortNumber, WireData>)
        : SimulationGraph =
    (graph, outputMap) ||> Map.fold (fun graph outPortNumber wireData ->
        match comp.Outputs.TryFind outPortNumber with
        | None -> failwithf "what? Reducer produced inexistent output portNumber %A in component %A" outPortNumber comp
        | Some targets ->
            // Trigger simulation step with the newly produced input in
            // every target.
            (graph, targets) ||> List.fold (fun graph (nextCompId, nextPortNumber) ->
                feedInput graph nextCompId (nextPortNumber, wireData)
            )
    )

/// Tells wether a component is clocked or not. Note that Custom components may
/// be clocked (cannot tell without recursively analysing them), so they are
/// considered colcked.
let private isClockedComponent comp =
    match comp.Type with
    | DFF | Custom _ -> true // We have to assume custom components are clocked as they may be.
    | _ -> false

/// Send one global clock tick to all clocked components, and return the updated
/// simulationGraph.
let feedClockTick (graph : SimulationGraph) : SimulationGraph =
    // Take a snapshot of each clocked component with its inputs just before the
    // clock tick.
    let clockedCompsBeforeTick =
        graph |> Map.filter (fun _ comp -> isClockedComponent comp)
    // For each clocked component, feed the clock tick together with the inputs
    // snapshotted just before the clock tick.
    (graph, clockedCompsBeforeTick) ||> Map.fold (fun graph compId comp ->
        let reducerInput = {
            Inputs = comp.Inputs
            CustomSimulationGraph = comp.CustomSimulationGraph
            IsClockTick = true
        }
        let reducerOutput = comp.Reducer reducerInput
        match reducerOutput.Outputs with
        | None -> failwithf "what? A clocked component should ALWAYS produce outputs after a clock tick: %A" comp
        | Some outputMap ->
            // The component may have changed in other iterations of this cycle,
            // make sure we get the most recent version so to not override these
            // changes when readding comp to the graph.
            // For example, if there are two DFF in series, and this loop
            // updates the second one first, it will have a new input. If we
            // use the snapshotted comp and we readd it to the graph, then we
            // lose the information about the new input.
            // Therefore we need to:
            // - re-read the most update comp from the graph.
            // - update the custom simulation graph of this comp with the
            //   reducer output.
            // - readd the all new comp to the graph.
            // Note that updating the CustomSimulationGraph is necessary since
            // we may be dealing with custom clocked components, which means
            // the feedClockTick operaion changes the graph of that custom
            // component.
            let comp = match graph.TryFind comp.Id with
                       | None -> failwith "what? Impossible case in feedClockTick"
                       | Some comp -> comp
            let comp = { comp with CustomSimulationGraph = reducerOutput.NewCustomSimulationGraph }
            let graph = graph.Add (comp.Id, comp)
            // Feed the newly produced outputs into the combinational logic.
            feedReducerOutput comp graph outputMap
    )

/// Feed zero to a simulation input.
/// This function is supposed to be used with Components of type Input.
let feedSimulationInput graph inputId wireData =
    feedInput graph inputId (InputPortNumber 0, wireData)

/// Feed zeros to all simulation inputs, and feed a single clock tick.
/// This way all combinational logic has been touched once and had produced its
/// outputs.
let InitialiseGraphWithZeros
        (inputIds : SimulationIO list)
        (graph : SimulationGraph)
        : SimulationGraph =
    // Feed a clock tick to initialize all of the nets that are after clocked
    // components, which cannot be initialised by just feeding the inputs.
    let graph = feedClockTick graph
    // Feed zero to all simulation inputs.
    (graph, inputIds) ||> List.fold (fun graph (inputId, _, width) ->
        let data = List.replicate width Zero
        feedSimulationInput graph inputId data
    )

/// Given a list of IO nodes (i.e. Inputs or outputs) extract their value.
/// If they dont all have a value, an error is thrown.
let extractSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * WireData) list =
    let extractWireData (inputs : Map<InputPortNumber, WireData>) : WireData =
        match inputs.TryFind <| InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some bit -> bit
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel, width) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> ((ioId, ioLabel, width), extractWireData comp.Inputs) :: result
    )

/// Simlar to extractSimulationIOs, but do not fail if a bit is not set, just
/// ignore it.
let extractIncompleteSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * WireData) list =
    let extractWireData (inputs : Map<InputPortNumber, WireData>) : WireData option =
        inputs.TryFind <| InputPortNumber 0
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel, width) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel, width)
        | Some comp -> match extractWireData comp.Inputs with
                       | None -> result
                       | Some wireData -> ((ioId, ioLabel, width), wireData) :: result
    )

/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes.
let getSimulationIOs
        (components : Component list)
        : SimulationIO list * SimulationIO list =
    (([], []), components) ||> List.fold (fun (inputs, outputs) comp ->
        match comp.Type with
        | Input w  -> ((ComponentId comp.Id, ComponentLabel comp.Label, w) :: inputs, outputs)
        | Output w -> (inputs, (ComponentId comp.Id, ComponentLabel comp.Label, w) :: outputs)
        | _ -> (inputs, outputs)
    )

/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes in a simulationGraph.
let getSimulationIOsFromGraph
        (graph : SimulationGraph)
        : SimulationIO list * SimulationIO list =
    (([], []), graph) ||> Map.fold (fun (inputs, outputs) compId comp ->
        match comp.Type with
        | Input w  -> ((comp.Id, comp.Label, w) :: inputs, outputs)
        | Output w -> (inputs, (comp.Id, comp.Label, w) :: outputs)
        | _ -> (inputs, outputs)
    )
