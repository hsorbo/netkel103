namespace NetKel103.Unfinished
open System
open System.Net
open System.Text
open System.Net.Sockets
open NetKel103.Wire
// https://www.eevblog.com/forum/testgear/review-of-the-korad-kel103-programmable-load/?all

module UdpUtils =
    let read (udpClient:UdpClient) = 
        let mutable local = IPEndPoint(0, 0)
        try
            //Some(local, udpClient.Receive(&local))
            udpClient.Receive(&local) |> Some
        with 
            | :? SocketException as ex when ex.ErrorCode = 60 -> None
    
    let readMany udpClient =
        Seq.initInfinite (fun _ -> read udpClient) |> Seq.takeWhile Option.isSome |> Seq.choose id

    //let readManyStrings (encoding:Text.Encoding) udpClient = readMany udpClient |> Seq.map encoding.GetString
    //let readManyAscii x = readManyStrings Encoding.ASCII x
module NetworkDetect = 
    let detect (broadcastAddress : IPAddress) =
        let encoding =  Encoding.ASCII
        use udpClient = new UdpClient()
        udpClient.Client.Bind(IPEndPoint(IPAddress.Any, 18191))
        udpClient.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.Broadcast, 1);
        udpClient.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.DontRoute, 1);
        let s = encoding.GetBytes "find_ka000"
        udpClient.Send(s, IPEndPoint(broadcastAddress, 18191)) |> ignore
        udpClient.Client.ReceiveTimeout <- 1000;
        udpClient 
            |> UdpUtils.readMany 
            |> Seq.map encoding.GetString 
            |> Seq.map (fun x -> x.Split("\n", StringSplitOptions.TrimEntries))
            |> Seq.filter (fun x -> x.Length = 4)
            |> List.ofSeq 

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
        let result = 
            udpClient
            |> UdpUtils.readMany
            |> Seq.map encoding.GetString
            |> Seq.toList
        String.Join("", result).Trim()

    member _.Query cmd =
        let info = toInfo cmd
        let definition = toDef info
        if CommandType.canQuery info.Type |> not then failwith "Cant query"
        send definition.Query
        udpClient
            |> UdpUtils.readMany
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

