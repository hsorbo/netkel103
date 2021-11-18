open System
open Argu
open NetKel103.Unfinished
open NetKel103.Wire

open System.Net
open Microsoft.FSharp.Reflection
//detect https://github.com/rogersstuart/KEL103Driver/blob/master/KEL103Driver/KEL103Tools.cs

type Measure =
    | Voltage = 1
    | Current = 2
    | Power = 3
    | Running = 4
    | BatteryCapacity = 5

let queryCommands = knownCommands |> List.filter (fun x -> CommandType.canQuery x.Type)

type Arguments =
    | Ip of host:string * port:int
    | Serial of serial:string * baud:int
    | Get of string list
    | Repl
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Repl -> "REPL"
            | Serial _ -> "Serial connection"
            | Ip _ -> "Network connection"
            | Get _ -> 
                let join (separator:string) (strings: string seq) = String.Join(separator, strings)
                queryCommands |> List.map (fun x -> sprintf "%A" x.Command) |> join "\n"


let repl (client:ExperimentalUdpClient) =
    ReadLine.AutoCompletionHandler <- { 
        new IAutoCompleteHandler with 
            member _.Separators with get () = [|'A'|] and set(value) = ()
            member _.GetSuggestions(text:string, index:int) : string array = 
                knownCommands 
                    |> List.filter (fun x -> x.Raw.StartsWith(text)) 
                    |> List.map (fun x -> x.Raw) 
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

let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None
        

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
    
    if(cmd.Contains(Repl)) then
        repl client
    elif(cmd.Contains(Get)) then
        let getList = cmd.TryGetResult(Get) |> Option.defaultValue List.empty
        
        let getList' = 
            if getList |> List.contains "all" 
            then queryCommands |> List.map (fun x -> x.Command)
            else getList |> List.map fromString<Commands> |> List.choose id |> List.distinct
        for cmd in getList' do
            let response = client.Query(cmd)
            let r = 
                match response with
                | FloatWithUnitValue(x, d) -> sprintf "%g%A"x d
                | OnOffValue x -> sprintf "%s" (if x = On then "on" else "off")
                | StringValue x -> sprintf "%s" x
                | Nothing -> sprintf "Nothing"
                | _ -> sprintf "Error %A" response
            printfn "%A: %s" cmd r
        ()
    0