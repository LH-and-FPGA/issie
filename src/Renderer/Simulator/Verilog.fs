﻿module Verilog

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open EEExtensions
open Fast
open Helpers
open NumberHelpers

type VWireType =
    | VlogInput of int
    | VlogOutput of int
    | VlogRegister of int
    | VlogComb of int

/// write a guaranteed globally unique name for each Verilog output
let addUniqueVerilogNames (fs: FastSimulation) =
    /// convert an index into an equivalent alphabetic suffix
    let numToChars (root: char) n =
        $"%d{n}"
        |> Seq.map (fun ch -> int ch + int root - int '0' |> char |> string)
        |> String.concat ""

    /// these are the components we need to give disambuated outputs
    let activeComps = 
        fs.FComps 
        |> mapValues
        |> Array.filter (fun fc -> fc.Active)

    /// take FullName and convert it into a verilog compatible form
    let verilogNameConvert (s:string) = 
        EEExtensions.String.split [|'('|] s
        |> Array.toList
        |> function | h :: _ -> h | [] -> ""
        |> String.replace "." "_" 
        |> String.replace " " "_"

    /// Make sure all names are unique by appending to names disambiguating characters
    /// don't allow "" as a name, so similarly append to that.
    let rec disambiguate (root: char) (nameL: ('a * string) array) =
        let groups =
            nameL
            |> Array.groupBy (fun (fc, vName) -> vName)
        match groups with 
        | _ when groups.Length = nameL.Length -> nameL
        | groups ->
            groups
            |> Array.collect (fun (vName, fcA) ->
                fcA
                |> Array.mapi (fun i (fc,vName) -> 
                    match i with
                    | 0
                    | 0 -> fc, vName
                    | i -> fc, vName + numToChars root i))
                // recursively disambiguate in case we have created a new name clash
            |> disambiguate root

    let uniqueNames =
        activeComps
        |> Array.map (fun fc -> fc, verilogNameConvert fc.FullName)
        |> Array.map (function |(fc,"")-> fc,"NULL" | x -> x)
        |> disambiguate 'a'
        |> Array.collect ( fun (fc,vName) ->
            [|0..fc.Outputs.Length-1|]
            |> Array.map ( fun i ->
                match fc.FType, fc.AccessPath with
                | Output _,[] |Input _,[] ->
                    ((fc, OutputPortNumber i), $"{vName}" )
                | _ -> ((fc, OutputPortNumber i), $"{vName}_out{i}" )))
        |> disambiguate 'q'              
    uniqueNames
    |> Array.iter (fun ((fc, OutputPortNumber n), name) ->
        fc.VerilogOutputName.[n] <- name)
        

let makeAsyncRomModule (moduleName: string) (mem: Memory) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1 <<< mem.AddressWidth
    let romInits = 
        [|0..numWords-1|]
        |> Array.map (fun a -> match Map.tryFind (int64 a) mem.Data with | None -> a, 0uL | Some x -> a, uint64 x)
        |> Array.map (fun(a,d)-> sprintf $"rom[%d{a}] = %d{d};")
        |> String.concat "\n"

    sprintf $"""

    module rom_single(q, a);
    output[%d{dMax}:0] q;
    input [%d{aMax}:0] a;

    wire [%d{dMax}:0] rom [%d{numWords-1}:0];
    assign q <= rom[a]
    initial
    begin
        %s{romInits}
    end
     """

let makeRamModule (moduleName: string) (mem: Memory) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1u <<< mem.AddressWidth
    let ramInits = 
        mem.Data
        |> Map.toArray
        |> (Array.map (fun(a,d)-> sprintf $"ram[%d{a}] = %d{d};"))
        |> String.concat "\n"

    sprintf $"""

    module ram_single(q, a, d, we, clk);
    output[%d{dMax}:0] q;
    input [%d{dMax}:0] d;
    input [%d{aMax}:0] a;
    input we, clk;
    reg [%d{dMax}:0] mem [%d{numWords}:0];
     always @(posedge clk) begin
         if (we)
             mem[a] <= d;
         q <= mem[a];
     end    

    initial
    begin
        %s{ramInits}
    end
    endmodule    
    
    """

let activeComps (fs:FastSimulation) =
    [
        fs.FConstantComps
        fs.FClockedComps
        fs.FOrderedComps
    ]
    |> Array.concat

let makeAccessPathIndex (fs: FastSimulation) =
    let apArr = Array.append [|[]|] (activeComps fs |> Array.map (fun fc -> fc.AccessPath))
    apArr
    |> Array.distinct
    |> Array.sortBy (fun ap -> List.length ap)
    |> Array.indexed
    |> Array.map (fun (index,ap) -> ap,index)
    |> Map.ofArray



let wireType (fc:FastComponent) (OutputPortNumber opn: OutputPortNumber) =
    match fc.FType, fc.AccessPath with
    | Input n,[] -> VlogInput n
    | Output n, [] -> VlogOutput n
    | DFF,_ | DFFE,_ -> VlogComb 1
    | Register n,_ | RegisterE n, _ -> VlogRegister n
    | _ ->
        match fc.OutputWidth.[opn] with
        | Some n -> VlogComb n
        | None -> failwithf "No output width found for %A output %d" fc.FullName opn

let getWire (wType: VWireType) (name: string) =
    let make s n = sprintf $"%s{s} %s{name};\n"
    match wType with
    | VlogInput n -> make "input" n
    | VlogOutput n -> make "output" n
    | VlogComb n -> make "wire" n
    | VlogRegister n -> make "reg" n


let getInstanceOf (block:string) (instanceName:string) (ports: string array) =
    let portNames = ports |> String.concat ","
    sprintf $"%s{block} %s{instanceName} (%s{portNames});\n"
    
let getVerilogBinaryOp cType op1 op2 =
    let bin opS = sprintf "%s %s %s" op1 opS op2
    let not exp = sprintf "!(%s)" exp
    match cType with
    | And -> bin "&&"
    | Or -> bin "||"
    | Nand -> not <| bin "&&"
    | Nor -> not <| bin "||"
    | Xor -> sprintf "((%s && !%s) || (!%s) && %s)" op1 op2 op1 op2
    | Xnor -> sprintf "!((%s && !%s) || (!%s) && %s)" op1 op2 op1 op2
    | _ -> failwithf "operator %A not defined" cType

let makeBits w (c:uint64) =
    sprintf $"%d{w}'h%x{c}"



let getVPortOut (fc:FastComponent) (OutputPortNumber opn) =
    fc.VerilogOutputName.[opn]

let getVPortOutWithSlice (fc:FastComponent) (opn: OutputPortNumber) =
    let name = getVPortOut fc opn
    let (OutputPortNumber n) = opn
    let width = Option.get fc.OutputWidth.[n]
    match width with
    | 1 ->  $"{name}"
    | _ -> $"[{width-1}:0] {name}"

let getVPortInput (fs: FastSimulation) (fc:FastComponent) (InputPortNumber ipn): string = 
    let labBase = fc.FullName
    match fc.InputDrivers.[ipn] with
    | Some (fid,opn) -> getVPortOut fs.FComps.[fid] opn
    | None -> failwithf "Can't find input driver for %A port %d"  fc.FullName ipn

let getVerilogComponent (fs: FastSimulation) (fc: FastComponent) =
    let ins i = getVPortInput fs fc (InputPortNumber i)
    let outs i = getVPortOut fc (OutputPortNumber i)

    let outW i = 
        match fc.OutputWidth.[i] with
        | Some n -> n
        | None -> failwithf "Can't find output width for output port %d of %A\n" i fc.FullName

    let inW i =
        let (fid, OutputPortNumber opn) = 
            match fc.InputDrivers.[i] with 
            | Some x -> x
            | None -> failwithf "Can't find input driver for port %d of %s" i fc.FullName
        fs.FComps.[fid].OutputWidth.[opn]
        |> function | Some n -> n 
                    | None -> failwithf 
                                "Can't find output width for output port %d of %A\n" 
                                opn 
                                fs.FComps.[fid]

    match fc.FType, fc.AccessPath with
    | Input _, [] -> failwithf "What? cannot call getVerilogComponent to find code for global Input"
    | Output _, _ 
    | IOLabel _, _ | Input _,_-> sprintf $"assign %s{outs 0} = %s{ins 0};\n"
    | _ ->          
        match fc.FType with
        | Not ->
            sprintf "assign %s = ! %s;\n" (outs 0) (ins 0)
        | And | Or | Xor | Nand | Nor | Xor -> 
            sprintf "assign %s = %s;\n" (outs 0) (getVerilogBinaryOp fc.FType (ins 0) (ins 1))
        | DFF | DFFE | Register _ | RegisterE _ ->
            $"always @(posedge clk) %s{outs 0} <= %s{ins 0};\n"
        | Constant(w,c) ->
            $"assign %s{outs 0} = %s{makeBits w (uint64 (uint32 c))};\n"
        | Decode4 ->
            let w = outW 1
            $"assign %s{outs 0} = (%s{ins 0} == 2'b00) ? %s{ins 1} : {makeBits w (uint64 0)};\n" +
            $"assign %s{outs 1} = (%s{ins 0} == 2'b01) ? %s{ins 1} : {makeBits w (uint64 0)};\n" +
            $"assign %s{outs 2} = (%s{ins 0} == 2'b10) ? %s{ins 1} : {makeBits w (uint64 0)};\n" +
            $"assign %s{outs 3} = (%s{ins 0} == 2'b11) ? %s{ins 1} : {makeBits w (uint64 0)};\n"
        | Demux2 ->
            let w = outW 0
            $"assign %s{outs 0} = %s{ins 1} ? {makeBits w (uint64 0)} : %s{ins 0};\n" +
            $"assign %s{outs 1} = %s{ins 1} ? %s{ins 0} : {makeBits w (uint64 0)};\n"
        | NbitsAdder n ->
            let cin = ins 0
            let a = ins 1
            let b = ins 2
            let sum = outs 0
            let cout = outs 1
            sprintf "%s" ("assign {" + $"%s{cout},%s{sum}" + "} = " + $"%s{a} + %s{b} + %s{cin};\n")
        | NbitsXor n ->
            let a = ins 0
            let b = ins 1
            let xor = outs 0
            sprintf $"assign %s{xor} <= %s{a} ^ %s{b};\n"
        | Mux2 ->
            sprintf $"assign %s{outs 0} = %s{ins 2} ? %s{ins 1} : %s{ins 0};\n"
        | BusSelection(outW,lsb) ->
            let sel = sprintf "[%d:%d]" (outW+lsb-1) lsb
            sprintf $"assign %s{outs 0} = %s{ins 0}%s{sel};\n"
        | BusCompare(w,c) ->
            sprintf $"assign %s{outs 0} = %s{ins 0} == %s{makeBits w (uint64 (uint32 c))};\n"
        | MergeWires ->
            sprintf "assign %s = {%s,%s};\n" (outs 0) (ins 0) (ins 1)
        | SplitWire _ ->
            let lsbBits = outW 0
            let msbBits = outW 1
            sprintf $"assign %s{outs 0} = %s{ins 0}[%d{lsbBits-1}:0];\n" +
            sprintf $"assign %s{outs 1} = %s{ins 0}[%d{msbBits+lsbBits-1}:%d{msbBits}];\n"
        | AsyncROM mem ->
            let modName = fc.FullName + "__ROM"
            makeAsyncRomModule modName mem
        | ROM mem ->
            ""
        | RAM mem -> 
            let modName = fc.FullName + "__RAM"
            makeRamModule modName mem    
        | Custom _ -> 
            failwithf "What? custom components cannot exist in fast Simulation data structure"
        | _ -> failwithf "What? impossible!: fc.FType =%A" fc.FType

let getInstantiatedModules (fs:FastSimulation) = [||]

let getMainHeader (fs:FastSimulation) =
    Array.append 
        fs.FGlobalInputComps 
        (Array.filter (fun fc -> isOutput fc.FType && fc.AccessPath = []) fs.FOrderedComps)
    |> Array.collect (fun fc -> 
        match fc.FType, fc.AccessPath with
        | Input _, [] | Output _, [] -> 
            [|fc.VerilogOutputName.[0]|]
        | _ -> [||])
    |> String.concat ",\n\t"
    |> sprintf "module topLevel (\n\tclk,\n\t%s);"
    |> fun s -> [| s |]

let getMainSignalDefinitions (fs:FastSimulation) =
    fs.FComps
    |> mapValues
    |> Array.filter (fun fc -> fc.Active)
    |> Array.collect (fun fc ->
        fc.Outputs
        |> Array.mapi (fun i opn ->
            let wt = wireType fc (OutputPortNumber i)
            getWire wt (getVPortOutWithSlice fc (OutputPortNumber i))))
    |> Array.sort
    |> Array.append [|"input clk;\n"|]

let getMainHardware (fs:FastSimulation) =
    let hardware =
        [|
            fs.FClockedComps
            fs.FOrderedComps
        |] |> Array.concat
    Array.map (getVerilogComponent fs) hardware
    
let getVerilog (fs:FastSimulation) =
    /// make sure we have Ok names to use for output
    printfn "%d components" fs.FComps.Count
    addUniqueVerilogNames fs
    [|
        getInstantiatedModules fs
        getMainHeader fs
        getMainSignalDefinitions fs
        getMainHardware fs
        [| "endmodule\n" |]
    |]
    |> Array.map (String.concat "")
    |> String.concat "\n"


   

