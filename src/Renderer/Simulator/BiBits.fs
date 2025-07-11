module BiBits

//------------------------------------------------------------------------------//
//-------------------EXPERIMENTAL - new data structure to replace WireData------//
//------------------------------------------------------------------------------//


// Currently this code is not used, because it is experimental and not yet implemented in the project.


type BitInt = uint32 array


/// get a field of bits width 'width' offset by 'offset', return the field with 0 offset
let inline getUpperField (x: uint32) (width: int) (offset: int) : uint32 =
    (x >>> offset) &&& ((1u <<< width) - 1u)

/// get the lower 'width' bits, return then offset by 'offset' bits
let inline getLowerField (x: uint32) (width: int) (offset: int) : uint32 =
    (x &&& ((1u <<< width) - 1u)) <<< offset

let floatCarryBit = Seq.init 32 (fun _ -> 2.) |> Seq.reduce (*)
