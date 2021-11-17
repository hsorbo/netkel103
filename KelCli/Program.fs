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
    | Repl
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Sample _ -> "Sample measurement(s)"
            | Repl -> "REPL"
            | Serial _ -> "Serial connection"
            | Ip _ -> "Network connection"


let sample (client:ExperimentalUdpClient) sampleList =
    if(sampleList |> List.isEmpty |> not) then 
        let measurements = [
            (Measure.Voltage, MeasureVoltage, "voltage", "V")
            (Measure.Current, MeasureAmp, "current", "A")
            (Measure.Power, MeasurePower, "watts", "W")
            (Measure.Running, Input, "running", "")
            (Measure.BatteryCapacity, BatteryCapacity, "batt-ah", "A")
        ]
        let a = 
            measurements
                |> List.filter (fun (x,_,_,_) -> List.contains x sampleList)
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

let repl (client:ExperimentalUdpClient) =
    ReadLine.AutoCompletionHandler <- { 
        new IAutoCompleteHandler with 
            member _.Separators with get () = [|'A'|] and set(value) = ()
            member _.GetSuggestions(text:string, index:int) : string array = 
                NetKel103.Wire.knownCommands 
                    |> List.map (fun (_,a,_) -> a) 
                    |> List.filter (fun x -> x.StartsWith(text)) 
                    |> List.toArray
        }
    let prompt _ = ReadLine.Read("SCPI>")
    printfn "Type exit to exit"
    Seq.initInfinite prompt
    |> Seq.takeWhile (fun x -> x <> "exit")
    |> Seq.iter (fun cmd ->
        ReadLine.AddHistory cmd
        client.Raw (sprintf "%s\n" cmd) |> printfn "%s"
        )
    

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
    
    let sampleList = cmd.TryGetResult(Sample) |> Option.defaultValue List.empty
    if(cmd.Contains(Repl)) then
        repl client
    elif(sampleList |> List.isEmpty |> not) then 
        sample client sampleList
    0