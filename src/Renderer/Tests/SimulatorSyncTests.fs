module SimulatorSyncTests

open DiagramTypes
open SimulatorTypes
open CanvasStatesSync

/// Tuple with: (diagramName, state, loadedComponents, number of clock ticks, inputs).
type private TestCaseInput = string * CanvasState * LoadedComponent list * int * (ComponentId * WireData) list
type private IterationOutput = (SimulationIO * WireData) list // Output after every clock tick.
type private TestCaseOutput = Result<IterationOutput list, SimulationError>
type private TestCase = string * TestCaseInput * TestCaseOutput

// The number of ticks that the test has to perform is given by the lenght of
// the iteration output list.

let testCasesSimulatorSync : TestCase list = [
    "Simple D-flip-flop, one input, one output (zero)",
    ("main", stateSync1, [], 5, [
        (ComponentId "6e7a2000-439c-108e-df6d-93cff7a41266", [Zero])
    ]),
    Ok (
        // Check it is zero for 5 ticks.
        [(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]
        |> List.replicate 5
    )

    "Simple D-flip-flop, one input, one output (one)",
    ("main", stateSync1, [], 5, [
        (ComponentId "6e7a2000-439c-108e-df6d-93cff7a41266", [One])
    ]),
    Ok (
        // Tick 0, out is zero.
        [[(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]]
        @
        // Then out is 1 four times.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [One]]
         |> List.replicate 4)
    )

    "Two D-flip-flop connected in series, one input, one output (one)",
    ("main", stateSync2, [], 5, [
        (ComponentId "3739e54a-fd21-bf60-8fc2-a3d10108c947", [One])
    ]),
    Ok (
        // Tick 0 and 1, out is zero.
        [ [(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]
          [(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]] ]
        @
        // Then out is 1 three times.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [One]]
         |> List.replicate 3)
    )
]
