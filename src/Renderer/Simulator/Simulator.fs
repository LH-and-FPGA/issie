(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open SimulationBuilder
open SimulationRunner
open DependencyMerger
open SimulationGraphAnalyser

// Simulating a circuit has four phases (not precisely in order of execution):
// 1. Building a simulation graph made of SimulationComponents.
// 2. Merging all the necessary dependencies.
// 3. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc...
// 4. Setting the values of the input nodes of the graph to kickstart the
//    simulation process.

/// Builds the graph and simulates it with all inputs zeroed.


/// upper case a string
let cap (sheet:string) = sheet.ToUpper()

/// look up a sheet in a set of loaded components
let getSheet (ldcs: LoadedComponent list) (openSheet: string)  =
    match List.tryFind (fun ldc -> cap ldc.Name = cap openSheet) ldcs with
    | None -> failwithf $"getSheet failed to look up '{openSheet}' in {ldcs |> List.map (fun ldc -> ldc.Name)}"
    | Some name -> name


let getDirectDependencies (cs:CanvasState) =
    fst cs
    |> List.collect (fun comp -> match comp.Type with | Custom ct-> [comp.Label, ct.Name] | _ -> [])

let childrenOf (ldcs: LoadedComponent list) (sheet:string) =
    getSheet ldcs sheet
    |> (fun ldc -> getDirectDependencies ldc.CanvasState)


/// sheets needed to simulate sheet with name sheet
let rec sheetsNeeded (ldcs: LoadedComponent list) (sheet:string): string list =
    let children = childrenOf ldcs sheet |> List.map snd
    children
    |> List.map (sheetsNeeded ldcs)
    |> List.concat
    |> List.append children
    |> List.append [sheet]
    |> List.distinct





let simulationIsValid (canvasState: CanvasState) (project: Project option) (fs:FastSimulation) =
    match project with
    | None -> 
        false
    | Some p -> 
        let openLDC = p.OpenFileName
        fs.SimulatedCanvasState
        |> List.forall (fun ldc -> 
            match openLDC = ldc.Name, List.tryFind (fun (ldc':LoadedComponent) -> ldc'.Name = ldc.Name) p.LoadedComponents with
            | _, None -> false
            | false, Some ldc' -> Extractor.loadedComponentIsEqual ldc ldc'
            | true, Some _ -> Extractor.stateIsEqual ldc.CanvasState canvasState)


    
let saveStateInSimulation (canvasState:CanvasState) (project: Project option) (fs:FastSimulation) =
    match project with
    | None -> 
        failwithf "What? Can't save state with project = None"
    | Some p -> 
        let ldcs = 
            let openLDC = p.OpenFileName
            let ldc' = Extractor.extractLoadedSimulatorComponent canvasState openLDC
            let ldcs = p.LoadedComponents |> List.map (fun ldc -> if ldc.Name = openLDC then ldc' else ldc)
            sheetsNeeded ldcs openLDC
            |> List.map (getSheet ldcs)
        {fs with SimulatedCanvasState= ldcs; SimulatedTopSheet = p.OpenFileName}

let rec prepareSimulation
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =

    /// Tune for performance of initial zero-length simulation versus longer run.
    /// Probably this is not critical.
    let initMaxSteps = 10
    match runCanvasStateChecksAndBuildGraph canvasState loadedDependencies with
    | Error err -> Error err
    | Ok graph ->
        match mergeDependencies diagramName graph
                                canvasState loadedDependencies with
        | Error err -> Error err
        | Ok graph ->
            // Simulation graph is fully merged with dependencies.
            // Perform checks on it.
            let components, connections = canvasState
            let inputs, outputs = getSimulationIOs components
            match analyseSimulationGraph diagramName graph connections with
            | Some err -> Error err
            | None -> 
                try
                    match FastRun.buildFastSimulation initMaxSteps graph with
                    | Ok fs -> 
                        Ok {
                            FastSim = fs                           
                            Graph = graph // NB graph is now not initialised with data
                            Inputs = inputs;
                            Outputs = outputs
                            IsSynchronous = hasSynchronousComponents graph
                            NumberBase = Hex
                            ClockTickNumber = 0
                        }
                    | Error  e -> Error  e
                with
                | AlgebraNotImplemented e -> Error e
                | e -> 
                    printfn "\nEXCEPTION:\n\n%A\n%A\n\n" e.Message e.StackTrace
                    Error {
                        Msg = sprintf "\nInternal ERROR in Issie fast simulation: %A\n\n%A\n" e.Message e.StackTrace
                        InDependency = None
                        ComponentsAffected = []
                        ConnectionsAffected = []
                    }
                |> Result.map (fun sd ->
                    //Fast.compareFastWithGraph sd |> ignore
                    sd)





/// Expose the extractSimulationIOs function from SimulationRunner.
let extractSimulationIOs = SimulationRunner.extractSimulationIOs

/// Get some info and the state of all stateful components in a graph.
let extractStatefulComponents
        (graph : SimulationGraph)
        : SimulationComponent list =
    graph
    |> Map.toList
    |> List.map snd
    |> List.filter (fun comp -> comp.State <> NoState)
    // TODO: recursively search custom components?


