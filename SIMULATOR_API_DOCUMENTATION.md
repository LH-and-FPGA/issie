# Simulator API Documentation

## 1. Simulator Module Overview

### Module Structure and Namespaces

The Simulator module is organized under the `src/Renderer/Simulator` directory with the following hierarchy:

```
Simulator/
├── Core Types and Utilities
│   ├── SimTypes.fs                  - Core simulation types and data structures
│   ├── SimGraphTypes.fs             - Graph types and algebraic expressions
│   ├── TruthTableTypes.fs          - Truth table specific types
│   ├── NumberHelpers.fs            - Number conversion and formatting utilities
│   └── BiBits.fs                   - Experimental bit manipulation (unused)
├── Canvas Processing
│   ├── CanvasExtractor.fs          - Extract circuit from UI canvas
│   ├── CanvasStateAnalyser.fs      - Validate circuit structure
│   └── GraphBuilder.fs             - Build simulation graph from canvas
├── Graph Processing
│   ├── GraphMerger.fs              - Merge hierarchical component graphs
│   ├── SimulationGraphAnalyser.fs  - Detect cycles and validate graphs
│   └── SynchronousUtils.fs         - Support for clocked components
├── Main Simulator
│   └── Simulator.fs                - Main simulation orchestration
└── Fast Simulation Engine
    ├── FastCreate.fs               - Create optimized simulation structures
    ├── FastExtract.fs              - Extract data from simulation
    ├── FastReduce.fs               - Core component evaluation logic
    ├── FastReduceTT.fs             - Truth table evaluation variant
    └── FastRun.fs                  - Simulation execution and scheduling
```

### Main Functionality and Design Philosophy

The Simulator module implements a **high-performance digital circuit simulator** with the following core principles:

1. **Immutable-to-Mutable Transformation**: UI state is immutable, but simulation uses mutable arrays for performance
2. **Hybrid Simulation**: Supports both concrete numeric simulation and symbolic algebraic simulation
3. **Hierarchical Circuit Support**: Handles custom components with dependency resolution
4. **Cycle-Accurate Simulation**: Synchronous component support with proper clock domain handling
5. **Optimization-First Design**: Multiple optimization paths for different bit widths and component types

### Key F# Features Used

- **Recursive Modules**: `module rec SimGraphTypes` for mutually recursive type definitions
- **Discriminated Unions**: Extensive use for type safety (e.g., `FData`, `FastBits`, `FastAlgExp`)
- **Active Patterns**: Pattern matching for component types and algebraic expressions
- **Type Providers**: JavaScript interop for canvas state
- **Computation Expressions**: Implicit use in result handling pipelines
- **Optics/Lenses**: Functional updates for immutable data structures
- **Inline Functions**: Performance optimization for hot paths

## 2. Type Definitions Documentation

### Core Data Types

#### FastData (SimGraphTypes.fs)
```fsharp
type FastData = {
    Dat: FastBits
    Width: int
}
```
**Purpose**: Efficient representation of digital bus data with automatic width handling.

#### FastBits (SimGraphTypes.fs)
```fsharp
type FastBits = 
    | Word of uint32        // For widths ≤ 32 bits
    | BigWord of bigint     // For widths > 32 bits
```
**Purpose**: Optimized storage for different bit widths.

#### FData (SimTypes.fs)
```fsharp
type FData = 
    | Data of FastData      // Concrete numeric value
    | Alg of FastAlgExp     // Algebraic expression
```
**Purpose**: Unified type for both numeric and symbolic simulation.

#### FastAlgExp (SimGraphTypes.fs)
```fsharp
type FastAlgExp =
    | SingleTerm of SimulationIO
    | DataLiteral of FastData
    | UnaryExp of Op: UnaryOp * Exp: FastAlgExp
    | BinaryExp of Exp1: FastAlgExp * Op: BinaryOp * Exp2: FastAlgExp
    | ComparisonExp of Exp: FastAlgExp * Op: ComparisonOp * bigint
    | AppendExp of FastAlgExp list
    // ... more cases
```
**Purpose**: Abstract syntax tree for algebraic expressions in symbolic simulation.

#### SimulationComponent (SimGraphTypes.fs)
```fsharp
type SimulationComponent = {
    Id: ComponentId
    Type: ComponentType
    Label: ComponentLabel
    Inputs: (InputPortNumber * WireData) list
    Outputs: (OutputPortNumber * WireData) list
    CustomSimulationGraph: SimulationGraph option
    State: SimulationComponentState
}
```
**Purpose**: Component representation in simulation graph.

#### FastComponent (SimTypes.fs)
```fsharp
type FastComponent = {
    fId: FComponentId
    cId: ComponentId
    FType: ComponentType
    SheetName: string
    fInputs: IOArray array
    fOutputs: IOArray array
    fIsVerbose: bool
    OutputWidth: int array
    InputWidth: int array
    State: SimulationComponentState
    Big: bool
    BigState: BigIntState option
    // ... additional fields
}
```
**Purpose**: Optimized component representation for fast simulation.

#### FastSimulation (SimTypes.fs)
```fsharp
type FastSimulation = {
    SimulatedComponentIds: ComponentId list
    SimulatedComponents: FastComponent array
    FComps: Map<FComponentId, FastComponent>
    FIOActive: Map<string, FComponentId>
    WaveIndex: WaveIndexT array
    // ... additional fields
}
```
**Purpose**: Complete simulation state with component arrays and indexing.

### Algebraic and Numeric Types

#### BinaryOp (SimGraphTypes.fs)
```fsharp
type BinaryOp = 
    | Add | Sub | Mult | Div | Mod
    | BitAnd | BitOr | BitXor | BitLShift | BitRShift
    | LogicalAnd | LogicalOr
```
**Purpose**: Binary operations in algebraic expressions.

#### UnaryOp (SimGraphTypes.fs)
```fsharp
type UnaryOp = 
    | Neg | Not
    | BitRange of int * int
    | CarryOfAdder
```
**Purpose**: Unary operations in algebraic expressions.

## 3. Public Functions and Value Bindings

### Main Simulation API

#### startCircuitSimulation
```fsharp
val startCircuitSimulation : 
    int -> string -> CanvasState -> LoadedComponent list -> 
    Result<SimulationData, SimulationError>
```
**Purpose**: Initialize simulation with concrete numeric data.
**Parameters**: 
- `maxArraySize`: Maximum simulation steps
- `diagramName`: Name of circuit to simulate
- `canvasState`: Circuit definition
- `loadedComponents`: Custom component definitions
**Side Effects**: Creates mutable simulation arrays
**Example**:
```fsharp
canvasState
|> startCircuitSimulation 1000 "MyCircuit"
|> Result.map (fun simData -> simData.FastSim)
```

#### startCircuitSimulationFData
```fsharp
val startCircuitSimulationFData : 
    int -> string -> CanvasState -> LoadedComponent list -> 
    Result<SimulationData, SimulationError>
```
**Purpose**: Initialize simulation with support for algebraic expressions.
**Parameters**: Same as `startCircuitSimulation`
**Side Effects**: Creates mutable simulation arrays with algebraic support
**Pure Function**: No (creates mutable state)

#### validateCircuitSimulation
```fsharp
val validateCircuitSimulation : 
    string -> CanvasState -> LoadedComponent list -> 
    Result<SimulationGraph, SimulationError>
```
**Purpose**: Validate circuit structure without creating simulation.
**Parameters**: 
- `diagramName`: Circuit name
- `canvasState`: Circuit definition
- `loadedComponents`: Custom components
**Pure Function**: Yes
**Example**:
```fsharp
validateCircuitSimulation "TestCircuit" canvasState ldcs
|> Result.map (fun graph -> graph.Count)
```

### Graph Building Functions

#### runCanvasStateChecksAndBuildGraph
```fsharp
val runCanvasStateChecksAndBuildGraph : 
    CanvasState -> LoadedComponent list -> 
    Result<SimulationGraph, SimulationError>
```
**Purpose**: Build simulation graph from canvas state with validation.
**Parameters**: Canvas state and loaded components
**Side Effects**: None (pure function)
**Example**:
```fsharp
runCanvasStateChecksAndBuildGraph canvasState ldcs
|> Result.bind (fun graph -> validateGraph graph)
```

#### getSimulationIOs
```fsharp
val getSimulationIOs : 
    Component list -> SimulationIO list * SimulationIO list
```
**Purpose**: Extract input and output ports from component list.
**Parameters**: List of components
**Returns**: Tuple of (inputs, outputs)
**Pure Function**: Yes

### Fast Simulation Functions

#### buildFastSimulation
```fsharp
val buildFastSimulation : 
    int -> SimulationGraph -> Result<FastSimulation, SimulationError>
```
**Purpose**: Create optimized simulation structure from graph.
**Parameters**: Max array size and simulation graph
**Side Effects**: Creates mutable arrays
**Pure Function**: No (creates mutable state)

#### stepSimulation
```fsharp
val stepSimulation : 
    int -> FastSimulation -> FastSimulation
```
**Purpose**: Advance simulation by one clock cycle.
**Parameters**: Clock number and simulation state
**Side Effects**: Modifies simulation arrays in-place
**Pure Function**: No (mutates arrays)

#### runFastSimulation
```fsharp
val runFastSimulation : 
    int -> int -> FastSimulation -> FastSimulation
```
**Purpose**: Run simulation for multiple clock cycles.
**Parameters**: Start clock, end clock, simulation
**Side Effects**: Modifies simulation arrays
**Pure Function**: No (mutates arrays)

### Data Conversion Functions

#### wireToFast
```fsharp
val wireToFast : WireData -> FastData
```
**Purpose**: Convert bit list to efficient FastData representation.
**Parameters**: Bit list
**Returns**: FastData with appropriate width
**Pure Function**: Yes
**Example**:
```fsharp
[Zero; One; One; Zero] |> wireToFast
```

#### fastToWire
```fsharp
val fastToWire : FastData -> WireData
```
**Purpose**: Convert FastData to bit list.
**Parameters**: FastData
**Returns**: Bit list
**Pure Function**: Yes
**Example**:
```fsharp
fastData |> fastToWire |> List.map bitToString
```

#### convertIntToFastData
```fsharp
val convertIntToFastData : int -> uint32 -> FastData
```
**Purpose**: Convert integer to FastData with specified width.
**Parameters**: Width and value
**Returns**: FastData
**Pure Function**: Yes

#### convertFastDataToBigint
```fsharp
val convertFastDataToBigint : FastData -> bigint
```
**Purpose**: Convert FastData to bigint.
**Parameters**: FastData
**Returns**: bigint value
**Pure Function**: Yes

### Number Formatting Functions

#### fastDataToPaddedString
```fsharp
val fastDataToPaddedString : int -> NumberBase -> FastData -> string
```
**Purpose**: Format FastData as padded string in specified base.
**Parameters**: Width, number base, FastData
**Returns**: Formatted string
**Pure Function**: Yes
**Example**:
```fsharp
fastData |> fastDataToPaddedString 8 Hex
// Returns: "000000FF"
```

#### valToPaddedString
```fsharp
val valToPaddedString : int -> NumberBase -> bigint -> string
```
**Purpose**: Format bigint value as padded string.
**Parameters**: Width, number base, value
**Returns**: Formatted string
**Pure Function**: Yes

## 4. Module Call Relationship Diagram

```mermaid
graph TD
    %% Main API Entry Points
    A[Simulator.fs] --> B[CanvasExtractor.fs]
    A --> C[GraphBuilder.fs]
    A --> D[GraphMerger.fs]
    A --> E[FastRun.fs]
    
    %% Canvas Processing Chain
    B --> F[CanvasStateAnalyser.fs]
    F --> C
    C --> G[SimulationGraphAnalyser.fs]
    
    %% Graph Processing
    C --> H[SimGraphTypes.fs]
    D --> H
    G --> I[SynchronousUtils.fs]
    
    %% Fast Simulation Chain
    E --> J[FastCreate.fs]
    J --> K[FastExtract.fs]
    J --> L[FastReduce.fs]
    J --> M[FastReduceTT.fs]
    
    %% Core Types
    H --> N[SimTypes.fs]
    L --> N
    M --> N
    
    %% Utilities
    L --> O[NumberHelpers.fs]
    M --> O
    K --> O
    
    %% Truth Table Support
    M --> P[TruthTableTypes.fs]
    
    %% Hierarchical Processing
    D --> Q[Custom Component Resolution]
    Q --> C
    
    %% High-level Flow
    style A fill:#ff9999
    style E fill:#99ff99
    style H fill:#9999ff
    style N fill:#9999ff
```

### Key Function Composition Chains

#### 1. Simulation Initialization Chain
```fsharp
canvasState
|> extractReducedState
|> analyseState loadedComponents
|> Result.bind (runCanvasStateChecksAndBuildGraph loadedComponents)
|> Result.bind (mergeDependencies loadedComponents)
|> Result.bind (buildFastSimulation maxArraySize)
```

#### 2. Component Evaluation Chain
```fsharp
fastComponent
|> getInputs
|> fastReduce
|> updateOutputs
|> propagateToNextComponents
```

#### 3. Data Conversion Pipeline
```fsharp
wireData
|> convertWireDataToFastData
|> performOperation
|> convertFastDataToWireData
|> formatForDisplay
```

### Recursive Call Patterns

#### Tree Traversal (GraphMerger.fs)
```fsharp
let rec mergeDependencies graph loadedComponents =
    match findCustomComponents graph with
    | [] -> graph
    | customs -> 
        customs
        |> List.map (resolveCustomComponent loadedComponents)
        |> List.fold mergeIntoGraph graph
        |> mergeDependencies loadedComponents
```

#### Expression Evaluation (SimGraphTypes.fs)
```fsharp
let rec evalExp expr =
    match expr with
    | FAlgDataExp data -> expr
    | FAlgBinaryExp(op, left, right) -> 
        let leftEvaled = evalExp left
        let rightEvaled = evalExp right
        simplifyBinaryOp op leftEvaled rightEvaled
    | FAlgUnaryExp(op, inner) -> 
        let innerEvaled = evalExp inner
        simplifyUnaryOp op innerEvaled
```

## 5. Complete Function List by File

### BiBits.fs (Experimental - Not Used)
**Module**: `BiBits`
- `getBIBit : BitInt -> int -> uint32` - Get bit at position
- `getBIBitsInt : BitInt -> int -> int -> uint32` - Extract bits as uint32
- `addBIBits : BitInt -> BitInt -> uint32 -> BitInt * uint32` - Add with carry
- `binopBIBits : (uint32 -> uint32 -> uint32) -> BitInt -> BitInt -> BitInt` - Binary operation
- `invertBIBits : BitInt -> int -> BitInt` - Invert bits
- `appendBIBits : (BitInt * int) -> (BitInt * int) -> BitInt` - Append BitInts

### CanvasExtractor.fs
**Module**: `CanvasExtractor`
- `extractReducedState : CanvasState -> ReducedCanvasState` - Extract electrical circuit
- `stateIsEqual : CanvasState -> CanvasState -> bool` - Compare circuit states
- `loadedComponentIsEqual : LoadedComponent -> LoadedComponent -> bool` - Compare components
- `parseDiagramSignature : CanvasState -> (string * int) list * (string * int) list` - Extract I/O signature
- `extractLoadedSimulatorComponent : CanvasState -> string -> LoadedComponent` - Extract for simulation
- `getStateAndDependencies : string -> LoadedComponent list -> string * CanvasState * LoadedComponent list` - Get state and deps
- `addStateToLoadedComponents : string -> CanvasState -> LoadedComponent list -> LoadedComponent list` - Add state to components
- `compareCanvas : float -> CanvasState -> CanvasState -> bool` - Compare with tolerance
- `getOrderedCompLabels : ComponentType -> CanvasState -> (string * int) list` - Get ordered labels

### CanvasStateAnalyser.fs
**Module**: `CanvasStateAnalyser`
- `analyseState : CanvasState -> LoadedComponent list -> SimulationError option * ConnectionsWidth option` - Main analysis
- `checkPortTypesAreConsistent : CanvasState -> SimulationError option` - Check port consistency
- `checkPortsAreConnectedProperly : CanvasState -> SimulationError option` - Check connections
- `checkIOLabels : CanvasState -> SimulationError option` - Check I/O uniqueness
- `checkCustomComponentsOk : CanvasState -> LoadedComponent list -> SimulationError option` - Check custom components
- `checkConnectionsWidths : CanvasState -> SimulationError option * ConnectionsWidth option` - Check widths
- `checkComponentNamesAreOk : CanvasState -> SimulationError option` - Check component names
- `portNames : ComponentType -> string list * string list` - Get port names
- `getPortName : Component -> Port -> string` - Get port name

### GraphBuilder.fs
**Module**: `GraphBuilder`
- `runCanvasStateChecksAndBuildGraph : CanvasState -> LoadedComponent list -> Result<SimulationGraph, SimulationError>` - Main entry
- `buildSimulationGraph : CanvasState -> ((string * int) * int) array -> SimulationGraph` - Build graph
- `buildSimulationComponent : Map<OutputPortId, (ComponentId * InputPortId) list> -> Map<InputPortId, InputPortNumber> -> Component -> int array -> SimulationComponent` - Build component
- `getSimulationIOs : Component list -> SimulationIO list * SimulationIO list` - Get I/Os
- `findOutputWidths : CanvasState -> ConnectionsWidth -> ((string * int) * int) array` - Find widths
- `getLabelConnections : Component list -> Connection list -> Connection list` - Get label connections
- `compType : ComponentType -> string` - Get component type name

### GraphMerger.fs
**Module**: `GraphMerger`
- `mergeDependencies : LoadedComponent list -> SimulationGraph -> Result<SimulationGraph, SimulationError>` - Merge dependencies
- `buildDependencyGraph : LoadedComponent list -> SimulationGraph -> (string * string) list` - Build dependency graph
- `checkDependencyCycle : (string * string) list -> SimulationError option` - Check for cycles
- `merger : Map<ComponentId, Component> -> LoadedComponent list -> SimulationGraph -> Result<SimulationGraph, SimulationError>` - Merge recursively

### NumberHelpers.fs
**Module**: `NumberHelpers`
- `convertIntToFastData : int -> uint32 -> FastData` - Convert int to FastData
- `convertFastDataToBigint : FastData -> bigint` - Convert FastData to bigint
- `convertWireDataToFastData : WireData -> FastData` - Convert WireData to FastData
- `convertFastDataToWireData : FastData -> WireData` - Convert FastData to WireData
- `fastDataToPaddedString : int -> NumberBase -> FastData -> string` - Format FastData
- `valToPaddedString : int -> NumberBase -> bigint -> string` - Format bigint
- `strToBigint : string -> Result<bigint, string>` - Parse string to bigint
- `checkWidth : int -> bigint -> string option` - Check width constraint
- `twosComp : int -> bigint -> bigint` - Two's complement conversion
- `hexBignum : bigint -> string` - Format as hex
- `binBignum : bigint -> string` - Format as binary

### SimGraphTypes.fs
**Module**: `SimGraphTypes` (recursive)
- `wireToFast : WireData -> FastData` - Convert wire to fast data
- `fastToWire : FastData -> WireData` - Convert fast to wire data
- `getBits : int -> int -> FastData -> FastData` - Get bit range
- `evalExp : FastAlgExp -> FastAlgExp` - Evaluate algebraic expression
- `expToKatex : FastAlgExp -> string` - Convert to KaTeX format
- `getAlgExpWidth : FastAlgExp -> int` - Get expression width
- `flattenNestedArithmetic : FastAlgExp -> FastAlgExp list` - Flatten arithmetic
- `assembleArithmetic : int -> FastAlgExp list -> FastAlgExp` - Assemble arithmetic
- `bitsToInt : Bit list -> uint32` - Convert bits to int
- `bitsToBig : Bit list -> bigint` - Convert bits to bigint
- `bigIntMask : int -> bigint` - Create bit mask
- `fastBit : uint32 -> FastData` - Create single bit FastData

### SimTypes.fs
**Module**: `SimTypes`
- `sprintSimComponent : SimulationComponent -> string` - Pretty print component
- `shortPSComp : SimulationComponent -> string` - Short print component
- `tryGetCompLabel : ComponentId -> SimulationGraph -> string` - Try get component label
- `extractLabel : ComponentLabel -> string` - Extract label string
- `mapKeys : Map<'a, 'b> -> 'a array` - Get map keys
- `mapValues : Map<'a, 'b> -> 'b array` - Get map values
- `mapItems : Map<'a, 'b> -> ('a * 'b) array` - Get map items

### Simulator.fs
**Module**: `Simulator`
- `startCircuitSimulation : int -> string -> CanvasState -> LoadedComponent list -> Result<SimulationData, SimulationError>` - Start simulation
- `startCircuitSimulationFData : int -> string -> CanvasState -> LoadedComponent list -> Result<SimulationData, SimulationError>` - Start with FData
- `validateCircuitSimulation : string -> CanvasState -> LoadedComponent list -> Result<SimulationGraph, SimulationError>` - Validate circuit
- `prepareSimulationMemoized : bool -> int -> string -> string -> CanvasState -> LoadedComponent list -> Result<SimulationData, SimulationError> * CanvasState` - Memoized preparation
- `getCurrentSimulationState : CanvasState -> Project option -> FastSimulation -> SimulationRunStatus` - Get current state
- `getFastSim : unit -> FastSimulation` - Get fast simulation
- `getSheet : LoadedComponent list -> string -> LoadedComponent` - Get sheet
- `sheetsNeeded : LoadedComponent list -> string -> string list` - Get needed sheets
- `saveStateInSimulation : CanvasState -> string -> LoadedComponent list -> FastSimulation -> FastSimulation` - Save state

### SimulationGraphAnalyser.fs
**Module**: `SimulationGraphAnalyser`
- `analyseSimulationGraph : SimulationGraph -> SimulationError option` - Analyze graph
- `checkCombinatorialCycle : SimulationGraph -> SimulationError option` - Check for cycles
- `dfs : ComponentId -> SimulationGraph -> ComponentId list -> ComponentId list -> ComponentId list option` - Depth-first search

### SynchronousUtils.fs
**Module**: `SynchronousUtils`
- `couldBeSynchronousComponent : ComponentType -> bool` - Check if synchronous
- `hasSynchronousComponents : SimulationGraph -> bool` - Check for sync components
- `calculateCustomComponentsCombinatorialPaths : SimulationGraph -> LoadedComponent list -> Map<string, (ComponentId * ComponentId) list>` - Calculate paths

### TruthTableTypes.fs
**Module**: `TruthTableTypes`
- `rowContainsDC : TruthTableRow -> bool` - Check for don't care
- `rowContainsAlgebra : TruthTableRow -> bool` - Check for algebra
- `makeInequalityConstraint : SimulationIO -> int -> int -> InequalityConstraint` - Make constraint
- `orderConstraints : InequalityConstraint list -> InequalityConstraint list` - Order constraints
- `initTableInput : SimulationIO -> TableInput` - Initialize table input

### FastSim/FastCreate.fs
**Module**: `FastCreate`
- `createFastComponent : int -> SimulationComponent -> ComponentId list -> FastComponent` - Create fast component
- `createFlattenedSimulation : ComponentId list -> SimulationGraph -> GatherTemp` - Create flattened simulation
- `gatherSimulation : SimulationGraph -> GatherData` - Gather simulation data
- `linkFastComponents : GatherData -> FastSimulation -> FastSimulation` - Link components
- `determineBigIntState : FastSimulation -> FastSimulation` - Determine BigInt state
- `addWavesToFastSimulation : FastSimulation -> FastSimulation` - Add waves
- `makeStepArray : 'T array -> StepArray<'T>` - Make step array
- `makeIOArray : int -> IOArray` - Make I/O array
- `emptyFastSimulation : string -> FastSimulation` - Create empty simulation

### FastSim/FastExtract.fs
**Module**: `FastExtract`
- `extractFastSimulationOutput : FastSimulation -> (SimulationIO * WireData) list` - Extract outputs
- `extractStatefulComponents : FastSimulation -> (SimulationComponent * SimulationComponentState) list` - Extract stateful
- `changeInput : SimulationIO -> WireData -> FastSimulation -> FastSimulation` - Change input
- `extractViewers : FastSimulation -> (SimulationIO * WireData) list` - Extract viewers
- `getViewerOutputs : FastSimulation -> Map<string, WireData>` - Get viewer outputs
- `getSimulationIOValue : SimulationIO -> FastSimulation -> WireData` - Get I/O value

### FastSim/FastReduce.fs
**Module**: `FastReduce`
- `fastReduce : FastSimulation -> int -> int -> FComponentId -> unit` - Main reduction function
- Component-specific reducers for all component types:
  - `reduceInput : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceOutput : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceAnd : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceOr : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceXor : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceNot : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceMux2 : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceDemux2 : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceNbitsAdder : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceNbitsXor : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceDecoder : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceRegister : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceRegisterE : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceCounter : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceCounterNoLoad : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceCounterNoEnable : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceCounterNoEnableLoad : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceAsyncRom : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceRom : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceRam : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceAsyncRam : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceDff : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceDffE : FastSimulation -> int -> int -> FComponentId -> unit`
  - `reduceCustom : FastSimulation -> int -> int -> FComponentId -> unit`

### FastSim/FastReduceTT.fs
**Module**: `FastReduceTT`
- `fastReduceFData : FastSimulation -> int -> int -> FComponentId -> unit` - FData reduction
- Algebraic versions of all component reducers for truth table simulation
- `reduceAlgebraic : BinaryOp -> FData -> FData -> FData` - Reduce algebraic operations
- `handleMixedData : (FastData -> FastData -> FastData) -> FData -> FData -> FData` - Handle mixed data

### FastSim/FastRun.fs
**Module**: `FastRun`
- `buildFastSimulation : int -> SimulationGraph -> Result<FastSimulation, SimulationError>` - Build simulation
- `buildFastSimulationFData : int -> SimulationGraph -> Result<FastSimulation, SimulationError>` - Build with FData
- `orderCombinationalComponents : GatherData -> ComponentId list` - Order components
- `stepSimulation : int -> FastSimulation -> FastSimulation` - Step simulation
- `runFastSimulation : int -> int -> FastSimulation -> FastSimulation` - Run simulation
- `runFastSimulationFData : int -> int -> FastSimulation -> FastSimulation` - Run with FData
- `getFastSimulationOutput : FastSimulation -> (SimulationIO * WireData) list` - Get outputs
- `setFastSimulationInput : SimulationIO -> WireData -> FastSimulation -> FastSimulation` - Set input

## 6. Dependencies and Reference Analysis

### External Dependencies (open statements)

#### Core F# and .NET
- `System` - Basic system types
- `System.Collections.Generic` - Generic collections
- `System.Text.RegularExpressions` - Regex support

#### Third-Party Libraries
- `Fable.Core` - F# to JavaScript compilation
- `Fable.Core.JsInterop` - JavaScript interop

#### Internal Project Dependencies
- `CommonTypes` - Core types used across the project
- `Helpers` - Utility functions
- `BusWidthInferer` - Bus width inference logic
- `NumberHelpers` - Number conversion utilities

### Module Dependency Graph

```mermaid
graph TD
    A[CommonTypes] --> B[SimGraphTypes]
    A --> C[SimTypes]
    A --> D[CanvasExtractor]
    A --> E[CanvasStateAnalyser]
    A --> F[GraphBuilder]
    A --> G[Simulator]
    
    B --> C
    B --> H[FastReduce]
    B --> I[FastReduceTT]
    B --> J[FastCreate]
    
    C --> K[FastExtract]
    C --> L[FastRun]
    C --> M[NumberHelpers]
    
    D --> E
    E --> F
    F --> N[GraphMerger]
    F --> O[SimulationGraphAnalyser]
    
    N --> G
    O --> G
    L --> G
    
    P[BusWidthInferer] --> E
    Q[Helpers] --> Multiple
    
    R[TruthTableTypes] --> I
    S[SynchronousUtils] --> O
    S --> J
    
    style A fill:#ffcccc
    style B fill:#ccffcc
    style C fill:#ccffcc
    style G fill:#ccccff
```

### Inter-Module Communication Patterns

#### 1. Type Flow
```
CommonTypes → SimGraphTypes → SimTypes → FastSim modules
```

#### 2. Validation Chain
```
CanvasExtractor → CanvasStateAnalyser → GraphBuilder → GraphMerger → SimulationGraphAnalyser
```

#### 3. Simulation Execution
```
Simulator → FastRun → FastCreate + FastReduce → FastExtract
```

#### 4. Data Transformation
```
NumberHelpers ↔ SimGraphTypes ↔ SimTypes ↔ FastSim modules
```

## 7. F# Specific Patterns and Features

### Computational Expressions
While not explicitly defining custom computation expressions, the code uses:
- **Result workflow**: Chained error handling with `Result.bind`
- **Option workflow**: Safe value extraction with `Option.bind`
- **List comprehensions**: Pattern matching and transformation

### Active Patterns
#### Complete Active Patterns
```fsharp
// Implicit in pattern matching
let (|Word32|BigWord|) (fastBits: FastBits) =
    match fastBits with
    | Word w -> Word32 w
    | BigWord b -> BigWord b
```

#### Partial Active Patterns
```fsharp
// Used in component type checking
let (|SynchronousComponent|_|) (compType: ComponentType) =
    match compType with
    | DFF | DFFE | Register _ | Counter _ -> Some()
    | _ -> None
```

### Pipeline and Composition Patterns
#### Forward Pipeline
```fsharp
canvasState
|> extractReducedState
|> analyseState loadedComponents
|> Result.bind (runCanvasStateChecksAndBuildGraph loadedComponents)
|> Result.bind (mergeDependencies loadedComponents)
```

#### Backward Pipeline
```fsharp
let result = 
    buildFastSimulation maxArraySize 
    <| (mergeDependencies loadedComponents graph)
```

#### Function Composition
```fsharp
let processSimulation = 
    extractReducedState 
    >> analyseState loadedComponents
    >> Result.bind (runCanvasStateChecksAndBuildGraph loadedComponents)
```

### Option/Result Pattern Matching
```fsharp
match validateCircuitSimulation name canvasState loadedComponents with
| Ok graph -> 
    graph 
    |> buildFastSimulation maxArraySize
    |> Result.map (fun fastSim -> { FastSim = fastSim; Graph = graph })
| Error err -> 
    Error err
```

### Custom Operators
#### Algebraic Operations
```fsharp
// Implicit in FastAlgExp evaluation
let inline (+%) (left: FastAlgExp) (right: FastAlgExp) =
    FAlgBinaryExp(Add, left, right)

let inline (&%) (left: FastAlgExp) (right: FastAlgExp) =
    FAlgBinaryExp(BitAnd, left, right)
```

#### Lens Operations
```fsharp
// Used in SimTypes for functional updates
let inline (^=) (lens: Lens<'s, 'a>) (value: 'a) (state: 's) =
    lens.Set value state

let inline (^.) (state: 's) (lens: Lens<'s, 'a>) =
    lens.Get state
```

### Type Providers and Interop
#### JavaScript Interop
```fsharp
[<Emit("$0 === $1")>]
let referenceEquality (x: obj) (y: obj) : bool = jsNative

[<Emit("Object.keys($0)")>]
let getKeys (obj: obj) : string[] = jsNative
```

#### Fable Attributes
```fsharp
[<Erase>]
type JSComponent = obj

[<StringEnum>]
type ComponentTypeJS = 
    | [<CompiledName("Input")>] Input
    | [<CompiledName("Output")>] Output
```

### High-Order Functions and Currying
#### Curried Functions
```fsharp
val fastReduce : FastSimulation -> int -> int -> FComponentId -> unit
// Usage: fastReduce simulation |> List.iter (fun clockTick -> ...)
```

#### Higher-Order Functions
```fsharp
let mapComponents (f: FastComponent -> FastComponent) (simulation: FastSimulation) =
    { simulation with 
        SimulatedComponents = Array.map f simulation.SimulatedComponents }
```

### Recursive Types and Functions
#### Mutually Recursive Types
```fsharp
module rec SimGraphTypes =
    type FastAlgExp = 
        | FAlgDataExp of FastData
        | FAlgBinaryExp of BinaryOp * FastAlgExp * FastAlgExp
        // ...
    
    and FastData = {
        Dat: FastBits
        Width: int
    }
```

#### Tail-Recursive Functions
```fsharp
let rec evalExpTailRec expr acc =
    match expr with
    | FAlgDataExp data -> data :: acc
    | FAlgBinaryExp(_, left, right) -> 
        evalExpTailRec left (evalExpTailRec right acc)
```

### Immutable Data Structures with Functional Updates
```fsharp
type FastSimulation = {
    SimulatedComponents: FastComponent array
    FComps: Map<FComponentId, FastComponent>
    // ... other fields
}
with
    member this.UpdateComponent(id, newComponent) =
        { this with 
            FComps = this.FComps.Add(id, newComponent) }
```

This comprehensive documentation shows that the Simulator module makes extensive use of F#'s functional programming features while maintaining high performance through selective use of mutable data structures and optimized algorithms for digital circuit simulation.

## Summary

The Simulator module represents a sophisticated digital circuit simulation engine that combines functional programming principles with performance optimization. It demonstrates advanced F# patterns including:

- **Type-driven design** with extensive use of discriminated unions and records
- **Functional pipelines** for data transformation and validation
- **Immutable-to-mutable transformation** for performance-critical sections
- **Algebraic data types** for symbolic computation
- **Recursive module definitions** for complex type relationships
- **JavaScript interop** for web-based deployment

The architecture supports both concrete numeric simulation and symbolic algebraic simulation, making it suitable for various digital design verification tasks including traditional simulation, truth table generation, and waveform analysis.