open System
open Argu
open NetKel103.Unfinished
open NetKel103.Wire

open System.Net

type Measure =
    | Volt = 1
    | Current = 2
    | Watts  = 3

type Arguments =
    | Ip of host:string * port:int
    | Serial of serial:string * baud:int
    | Sample of Measure list
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Sample _ -> "Sample measurement(s)"
            | Serial _ -> "Serial connection"
            | Ip _ -> "Network connection"

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "kelcli", errorHandler = errorHandler)
    let cmd = parser.ParseCommandLine ()  
    
    let c = 
        cmd.TryGetResult Ip 
        |> Option.map (fun (ip, port) -> IPEndPoint(IPAddress.Parse(ip), port))
        |> Option.get
    use client = new ExperimentalUdpClient(c)

    //  Seq.initInfinite (fun _ -> Console.ReadLine ()) 
    //    |> Seq.takeWhile (String.IsNullOrWhiteSpace >> not)
    //    |> Seq.map (sprintf "%s\n")
    //    |> Seq.map b.Raw
    //    |> Seq.iter (printfn "%s")
    match (client.Query(MeasureVoltage), client.Query(MeasureAmp), client.Query(MeasurePower)) with
    | (FloatWithUnitValue (v, _), FloatWithUnitValue (a, _), FloatWithUnitValue (w,_)) -> 
        printfn "%s %fA %fV %fW" (DateTime.Now.ToLongTimeString()) a v w
        0
    | _ -> 
        printfn "failed"
        1
