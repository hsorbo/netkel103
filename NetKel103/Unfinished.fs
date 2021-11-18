namespace NetKel103.Unfinished
open System
open System.Net
open System.Text
open System.Net.Sockets
open NetKel103.Wire

[<AutoOpen>]
module StringUtils = 
    let join (x:string) (y:string seq) = String.Join(x, y)
    let trim (s:string) = s.Trim()
    let split (splitOn:string) (s:string) = s.Split(splitOn, StringSplitOptions.RemoveEmptyEntries)
    let replace (old:string) (rep:string) (s:string) = s.Replace(old, rep)

module NetworkUtils =
    let read (udpClient:UdpClient) = 
        let mutable local = IPEndPoint(0, 0)
        try
            //Some(local, udpClient.Receive(&local))
            udpClient.Receive(&local) |> Some
        with 
            | :? SocketException as ex when ex.ErrorCode = 60 -> None
    
    let readMany udpClient =
        Seq.initInfinite (fun _ -> read udpClient) |> Seq.takeWhile Option.isSome |> Seq.choose id
    

    let searchNetkel () = seq {
        let encoding = Encoding.ASCII
        let src, dst = IPEndPoint(IPAddress.Any, 18191), IPEndPoint(IPAddress.Broadcast, 18191) 
        use udpClient = new UdpClient(src)
        udpClient.Send(encoding.GetBytes "find_ka000",dst) |> ignore
        udpClient.Client.ReceiveTimeout <- 1000;
        yield! udpClient 
            |> readMany 
            |> Seq.map encoding.GetString 
            |> Seq.map (replace "\000" "" >> split "\n")
            |> Seq.filter (fun x -> x.Length = 3)
            |> Seq.map (join " ")
        }   

type ExperimentalUdpClient (endpoint:IPEndPoint) =
    let encoding = Encoding.ASCII
    let udpClient = new UdpClient(endpoint.Port)
    do udpClient.Client.ReceiveTimeout <- 500
    //let local = IPEndPoint(IPAddress.Any, endpoint.Port)
    do udpClient.Connect(endpoint.Address, endpoint.Port)

    let send cmd = 
        let sendBytes = encoding.GetBytes(cmd:string)
        //printfn "sending: %s" (cmd.Trim()) 
        udpClient.Send(sendBytes, sendBytes.Length) |> ignore

    member _.Raw cmd =
        send cmd
        udpClient
            |> NetworkUtils.readMany
            |> Seq.map encoding.GetString
            |> Seq.toList
            |> (join "")
            |> trim

    member _.Query cmd =
        let info = toInfo cmd
        let definition = toDef info
        if CommandType.canQuery info.Type |> not then failwith "Cant query"
        send definition.Query
        udpClient
            |> NetworkUtils.readMany
            |> Seq.map encoding.GetString
            |> Seq.scan (+) ""
            |> Seq.skip 1
            |> Seq.filter definition.HappyWithResponse
            |> Seq.tryHead
            |> Option.defaultValue ""
            |> definition.ResponseHandler

    member _.Apply cmd arg =
        let definition = toDef cmd
        send (definition.GenerateRaw arg)

    interface IDisposable with member _.Dispose() = udpClient.Dispose()

