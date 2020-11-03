(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the CommonTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module rec ModelType

open CommonTypes
open MessageType
open SimulatorTypes
open Draw2dWrapper

//================================//
// Componenents loaded from files //
//================================//

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromSimulation : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromWaveSim : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromFiles : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromMemoryEditor : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromProperties : ((Msg -> unit) -> Fable.React.ReactElement) option
}


type AutoSaveT = Saving | Deleting | Inactive

type AsyncTasksT = {
    AutoSave: AutoSaveT
    /// time when last actually auto-saved
    LastAutoSave: Map<string,System.DateTime>
    /// time when system last checked canvas components with previous autosave value
    LastAutoSaveCheck: System.DateTime
    /// copy of what was last saved for real
    LastSavedCanvasState: Map<string,CanvasState>
    RunningSimulation: bool // placeholder - not used yet
    }

[<CustomEquality;NoComparison>]
type Model = {
    /// data used to peform auto-save
    AsyncActivity: AsyncTasksT
    /// All the data for waveform simulation (separate for each sheet)
    /// TODO: remove the simulation error.
    WaveSim : Map<string, WaveSimModel> * (SimulationError option)
    /// draw canvas
    Diagram : Draw2dWrapper
    /// if canvas is now different from that which is currently used by wave sim.
    WaveSimulationIsStale: bool
    /// canvas used for wave sim
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    /// used to determine whether current canvas has been saved (includes any change)
    LastDetailedSavedState: CanvasState
    /// component ids and connection ids currently selected
    LastSelectedIds: string list * string list
    CurrentSelected: Component list * Connection list
    /// last used bus width in bits - used as default in next component create dialog
    LastUsedDialogWidth: int
    /// component currently selected in properties dialog
    SelectedComponent : Component option // None if no component is selected.
    /// used by step simulator: simgraph for current clock tick
    CurrentSimulatorStep : Result<SimulationData,SimulationError> option // None if no simulation is running.
    /// which of the tabbed panes is currentlky visible
    RightTab : RightTab
    /// components and connections which are highlighted
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    /// Components and connections that have been selected and copied.
    Clipboard : CanvasState 
    /// Track the last added component
    CreateComponent : Component option 
    /// used to enable "SAVE" button
    SheetHasUnsavedChanges : bool
    /// true during period when a sheet or project is loading
    IsLoading: bool
    /// the project contains, as loadable components, the state of each of its sheets
    CurrProject : Project option
    /// function to create popup pane if present
    Popup : (PopupDialogData -> Fable.React.ReactElement) option
    /// data to populate popup (may not all be used)
    PopupDialogData : PopupDialogData
    /// record containing functions that create react elements of notifications
    Notifications : Notifications
    /// menu for sheets etc
    TopMenu : TopMenu
    /// used to determine whether mouse is currently dragging the divider, or used normally
    DragMode: DragMode
    /// viewer width in pixels altered by dragging the divider
    ViewerWidth: int
    /// TODO - delete this
    SimulationInProgress:  SimActionT option
    /// if true highlight connections from wavesim editor
    ConnsToBeHighlighted: bool
    /// true if wavesim scroll position needs checking
    CheckScrollPos: bool
} with
 
    override this.GetHashCode() =
        hash (reduce this)
        
    override this.Equals(x) = 
        match x with
        | :? Model as x' -> reduce this = reduce x'
        | _ -> false



let reduce (this: Model) = {|
         RightTab = this.RightTab
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         AsyncActivity = this.AsyncActivity
         SimulationIsStale = this.WaveSimulationIsStale
         LastSimulatedCanvasState = this.LastSimulatedCanvasState
         LastSelectedIds = this.LastSelectedIds
         CurrentSelected = this.CurrentSelected
         LastUsedDialogWidth = this.LastUsedDialogWidth
         SelectedComponent= this.SelectedComponent
         CreateComponent = this.CreateComponent
         HasUnsavedChanges = false
         CurrProject = match this.Popup with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         TopMenu = this.TopMenu
         DragMode = this.DragMode
         ViewerWidth = this.ViewerWidth
         SimulationInProgress = this.SimulationInProgress
         ConnsToBeHighlighted = this.ConnsToBeHighlighted

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightTab
         Clipboard = this.Clipboard
         CurrProject = match this.Popup with None -> false | _ -> true
         SimulationIsStale = this.WaveSimulationIsStale
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.CreateComponent
         HasUnsavedChanges = false
         CurrProject = match this.Popup with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         DragMode = this.DragMode
         ViewerWidth = this.ViewerWidth
         SimulationInProgress = this.SimulationInProgress
 |} 

/// Lens to facilitate changing AsyncActivity
let setActivity (f: AsyncTasksT -> AsyncTasksT) (model: Model) =
    {model with AsyncActivity = f model.AsyncActivity }




let getDetailedState (model:Model) =
    model.Diagram.GetCanvasState()
    |> Option.map Extractor.extractState
    |> Option.defaultValue ([],[])

let getReducedState (model:Model) =
    model.Diagram.GetCanvasState()
    |> Option.map Extractor.extractReducedState 

let addReducedState a name model =
    let lastState = a.LastSavedCanvasState
    match getReducedState model with
    | None -> lastState
    | Some state -> lastState.Add(name, state)


let changeSimulationIsStale (b:bool) (m:Model) = 
    { m with WaveSimulationIsStale = b}

let getComponentIds (model: Model) =
    let extractIds (jsComps,jsConns) = 
        jsComps
        |> List.map Extractor.extractComponent
        |> List.map (fun comp -> ComponentId comp.Id)
    model.Diagram.GetCanvasState()
    |> Option.map extractIds
    |> Option.defaultValue []
    |> Set.ofList

////////////////////////////
/// Saving WaveSim Model ///
////////////////////////////

/// get saveable record of waveform setup
let waveSimModel2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    let pars = wsMod.SimParams
    { 
        ClkWidth = pars.ClkWidth
        Cursor = pars.Cursor
        Radix = pars.Radix
        LastClk = pars.LastClk
        DisplayedPortIds = 
            dispPorts wsMod
            |> Array.map (fun p -> p.driverNet.[0].TargetCompId)
            |> Array.map (fun cId -> match cId with | ComponentId n -> n)
    }

/// setup current WaveSimModel from saved record
let savedWaveInfo2WaveSimModel (sWInfo: SavedWaveInfo) : WaveSimModel =
    { 
        InitWaveSimGraph = None
        AllPorts = Map.empty
        SimDataCache = [||]
        DispWaveSVGCache = {Top=[||]; Waves = Map.empty; Bottom = [||]}
        AllWaveNames = [||]
        SimParams = {
            DispNames = [||]
            ClkWidth = sWInfo.ClkWidth
            Cursor = sWInfo.Cursor
            Radix = sWInfo.Radix
            LastClk = sWInfo.LastClk
            LastScrollPos = None
        }
        WSState = { View=NoWS; NextView=None}
        LastCanvasState = None  
        CursorEmpty = false

    }

let getSheetWaveSimOpt (model:Model) : WaveSimModel option = 
    model.CurrProject
    |> Option.bind (fun p -> Map.tryFind p.OpenFileName (fst model.WaveSim))
    

let getSheetWaveSimErr (model:Model) =
    model.CurrProject
    |> Option.map (fun p -> snd model.WaveSim)
    |> Option.defaultValue None

let getSheetWaveCanvasState (model:Model) =
    getSheetWaveSimOpt model
    |> Option.map (fun (ws:WaveSimModel) -> ws.LastCanvasState)
    |> Option.defaultValue None

let getSheetWaveNetList (model:Model) =
    getSheetWaveCanvasState model
    |> Option.map Helpers.getNetList
   

//----------------------Print functions-----------------------------//
//------------------------------------------------------------------//


let spComp (comp:Component) =
    match comp.Type with
    | Custom {Name=name; InputLabels=il; OutputLabels=ol} -> sprintf "Custom:%s(ins=%A:outs=%A)" name il il
    | x -> sprintf "%A" x

let spConn (conn:Connection) = 
    sprintf "Conn:%A" conn.Vertices

let spState ((comps,conns):CanvasState) = 
    sprintf "Canvas<%A,%A>" (List.map spComp comps) (List.map spConn conns)

let spCanvas (model:Model) = 
    model.Diagram.GetCanvasState()
    |> Option.map Extractor.extractState
    |> Option.map spState
    |> Option.defaultValue "None"

let spComps comps =  
    sprintf "Comps%A" (List.map spComp comps)

let spOpt f thingOpt = match thingOpt with |None -> "None" | Some x -> sprintf "Some %s" (f x)

let spLdComp (ldc: LoadedComponent) =
    sprintf "LDC<%s:%A:%s>" ldc.Name ldc.TimeStamp ((fst >>spComps) ldc.CanvasState)

let spProj (p:Project) =
    sprintf "PROJ||Sheet=%s\n%s||ENDP\n" p.OpenFileName (String.concat "\n" (List.map spLdComp p.LoadedComponents))

let pp model =
    printf "\n%s\n%s" (spCanvas model) (spOpt spProj model.CurrProject)

let spMess msg =
    match msg with
    //| SetProject p -> sprintf "MSG<<SetProject:%s>>ENDM" (spProj p)
    //| SetLastSimulatedCanvasState canvasOpt-> sprintf "MSG<SetLastSimCanv:%s>>ENDM" (spOpt spState canvasOpt)
    | x -> sprintf "MSG<<%20A>>ENDM" x

let updateLdComps (name:string) (changeFun: LoadedComponent -> LoadedComponent)  (ldComps: LoadedComponent list)=
    ldComps
    |> List.map (fun ldc -> if ldc.Name=name then changeFun ldc else ldc)

let updateLdCompsWithCompOpt (newCompOpt:LoadedComponent option) (ldComps: LoadedComponent list) =
    match newCompOpt with 
    | None -> ldComps // no update
    | Some newComp -> 
        match List.tryFind (fun (ldc:LoadedComponent) -> ldc.Name = newComp.Name) ldComps with
        | None -> newComp :: ldComps
        | Some _ -> updateLdComps newComp.Name (fun _ -> newComp) ldComps

let getCurrFileWSMod(model: Model) =
    let wsMap = model.WaveSim
    match model.CurrProject with
    | None -> None
    | Some proj -> Map.tryFind proj.OpenFileName (fst wsMap)


let getCurrFileWSModNextView(model:Model) =
    getCurrFileWSMod model
    |> Option.bind (fun ws -> ws.WSState.NextView)


/// returns a string option representig the current file name if file is loaded, otherwise None
let getCurrFile (model: Model) =
    match model.CurrProject with
    | Some proj -> Some proj.OpenFileName
    | None -> None

let setCurrFileWSMod (ws: WaveSimModel) (model: Model) =
    match getCurrFile model with
    | Some fileName ->
        { model with WaveSim = Map.add fileName ws (fst model.WaveSim), 
                               snd model.WaveSim }
    | None -> model 


let updateCurrFileWSMod(updateFun: WaveSimModel -> WaveSimModel) (model: Model) =
    let wsMap = model.WaveSim
    match model.CurrProject with
    | None -> model
    | Some proj ->
        Map.tryFind proj.OpenFileName (fst wsMap)
        |> Option.map ( fun wsModel ->
                let ws' = updateFun wsModel
                setCurrFileWSMod ws' model)
        |> Option.defaultValue model
 