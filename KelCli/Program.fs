open System
open Argu
open NetKel103.Unfinished
open NetKel103.Wire

open System.Net
open Microsoft.FSharp.Reflection

let sndMap f (x, y) = (x, f y)
let fstMap f (x, y) = (f x, y)

let queryCommands =
    knownCommands
    |> List.filter (fun x -> CommandType.canQuery x.Type)

let setCommands =
    knownCommands
    |> List.filter (fun x -> CommandType.canSet x.Type)

type Arguments =
    | Ip of host: string * port: int
    | Serial of serial: string * baud: int
    | Get of string list
    | Set of key: string * value: string
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
            | Set _ ->
                setCommands
                |> List.map (fun x -> sprintf "%A" x.Command)
                |> join "\n"
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

    let queryResponse = getList' |> Seq.map (fun x -> (x, querier x))

    let colorize =
        function
        | FloatWithUnitValue (x, d) -> ConsoleColor.Blue
        | OnOffValue x ->
            match x with
            | On -> ConsoleColor.Green
            | Off -> ConsoleColor.Red
        | StringValue x -> ConsoleColor.Yellow
        | Nothing -> ConsoleColor.Gray
        | NumericValue x -> ConsoleColor.Magenta
        | ModeValue m -> ConsoleColor.Cyan

    if json |> not then
        queryResponse
        |> Seq.iter (fun (cmd, x) ->

            let prev = Console.ForegroundColor
            printf "%A: " cmd
            Console.ForegroundColor <- colorize x
            printfn "%s" (CommandValue.toString x)
            Console.ForegroundColor <- prev)
    else
        queryResponse
        |> Seq.map (fstMap (sprintf "%A"))
        |> Seq.map (sndMap (CommandValue.toPrimitives))
        |> Map.ofSeq
        |> System.Text.Json.JsonSerializer.Serialize
        |> printfn "%s"

let setter setter key value =
    match fromString<Commands> (key) with
    | None -> failwith "unknown command"
    | Some cmd ->
        let info = cmd |> toInfo

        match CommandType.argType info.Type with
        | None -> failwith "no set"
        | Some x -> setter cmd (CommandValue.fromString value x)

    ()

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
        elif (cmd.Contains(Set)) then
            match cmd.TryGetResult(Set) with
            | Some (k, v) -> setter (client.Set) k v
            | None _ -> printfn "unable to set"

        ()

    0
