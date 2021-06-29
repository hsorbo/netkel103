open System
open Argu
open NetKel103.Unfinished
open NetKel103.Wire

open System.Net

type Measure =
    | Voltage = 1
    | Current = 2
    | Power = 3
    | Running = 4
    | BatteryCapacity = 5

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
    
    let sample = cmd.TryGetResult(Sample) |> Option.defaultValue List.empty
    if(sample |> List.isEmpty |> not) then 
        let measurements = [
            (Measure.Voltage, MeasureVoltage, "voltage", "V")
            (Measure.Current, MeasureAmp, "current", "A")
            (Measure.Power, MeasurePower, "watts", "W")
            (Measure.Running, Input, "running", "")
            (Measure.BatteryCapacity, BatteryCapacity, "batt-ah", "A")
        ]
        let a = 
            measurements
                |> List.filter (fun (x,_,_,_) -> List.contains x sample)
                |> Seq.map (fun (_,cmd,c,d) -> (client.Query(cmd),c,d))
                //|> Seq.takeWhile (fun (x,_,_) -> )
                |> Seq.map (fun (response,c,d) -> 
                    match response with
                    | FloatWithUnitValue(x, _) -> sprintf "%s: %g%s" c x d
                    | OnOffValue x -> sprintf "%s: %s" c (if x = On then "on" else "off")
                    | _ -> sprintf "%s: Error" c
                )
                |> (fun x -> (String.Join(", ", x)))
        printfn "%s %s" (DateTime.Now.ToLongTimeString()) a
    0