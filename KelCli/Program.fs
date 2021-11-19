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

let queryCommands =
    knownCommands
    |> List.filter (fun x -> CommandType.canQuery x.Type)

type Arguments =
    | Ip of host: string * port: int
    | Serial of serial: string * baud: int
    | Get of string list
    | Json
    | Repl
    | Net_Detect
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Repl -> "REPL"
            | Serial _ -> "Serial connection"
            | Ip _ -> "Network connection"
            | Net_Detect -> "Search network"
            | Json -> "json"
            | Get _ ->
                queryCommands
                |> List.map (fun x -> sprintf "%A" x.Command)
                |> join "\n"

let fromString<'a> (s: string) =
    match FSharpType.GetUnionCases typeof<'a>
          |> Array.filter (fun case -> case.Name = s)
        with
    | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
    | _ -> None


let repl querier =
    ReadLine.AutoCompletionHandler <-
        { new IAutoCompleteHandler with
            member _.Separators = [| 'A' |]

            member _.Separators
                with set (value) = ()

            member _.GetSuggestions(text: string, index: int) : string array =
                knownCommands
                |> List.filter (fun x -> x.Raw.StartsWith(text))
                |> List.map (fun x -> x.Raw)
                |> List.toArray }

    let prompt _ = ReadLine.Read("SCPI>")
    printfn "Type exit to exit"

    Seq.initInfinite prompt
    |> Seq.takeWhile (fun x -> x <> "exit")
    |> Seq.iter (fun cmd ->
        ReadLine.AddHistory cmd
        querier (sprintf "%s\n" cmd) |> printfn "%s")

let query getList querier json =
    let getList' =
        if getList |> List.contains "all" then
            queryCommands |> List.map (fun x -> x.Command)
        else
            getList
            |> List.map fromString<Commands>
            |> List.choose id
            |> List.distinct

    let m f x = (x, f x)
    let sndf f (x, y) = (x, f y)
    let fstf f (x, y) = (f x, y)

    let queryResponse = getList' |> Seq.map (m querier)

    let format =
        function
        | FloatWithUnitValue (x, d) -> (ConsoleColor.Blue, box x)
        | OnOffValue x ->
            (if x = On then
                 (ConsoleColor.Green, true)
             else
                 (ConsoleColor.Red, false))
        | StringValue x -> (ConsoleColor.Yellow, x)
        | Nothing -> (ConsoleColor.Gray, null)
        | NumericValue x -> (ConsoleColor.Magenta, x)
        | ModeValue m -> (ConsoleColor.Cyan, m |> Mode.asString |> box)

    if json |> not then
        queryResponse
        |> Seq.map (sndf format)
        |> Seq.iter (fun (cmd, (color, resp)) ->
            let prev = Console.ForegroundColor
            printf "%A: " cmd
            Console.ForegroundColor <- color
            printfn "%A" resp
            Console.ForegroundColor <- prev)
    else
        queryResponse
        |> Seq.map (fstf (sprintf "%A"))
        |> Seq.map (sndf (format >> snd))
        |> Map.ofSeq
        |> System.Text.Json.JsonSerializer.Serialize
        |> printfn "%s"

[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<Arguments>(programName = "kelcli", errorHandler = errorHandler)

    let cmd = parser.ParseCommandLine()

    if (cmd.Contains(Net_Detect)) then
        printfn "Searching for KEL10x"

        NetworkUtils.searchNetkel ()
        |> Seq.iter (printfn "Found: %s")
    else
        let ip, port = cmd.GetResult Ip
        use client = new ExperimentalUdpClient(IPEndPoint(IPAddress.Parse(ip), port))

        if (cmd.Contains(Repl)) then
            repl client.Raw
        elif (cmd.Contains(Get)) then
            match cmd.TryGetResult(Get) with
            | Some x -> query x client.Query (cmd.Contains(Json))
            | None _ -> printfn "unable to get"

        ()

    0
