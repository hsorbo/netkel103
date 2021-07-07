namespace NetKel103.Unfinished
open System
open System.Net
open System.Text
open System.Net.Sockets
open NetKel103.Wire
// https://www.eevblog.com/forum/testgear/review-of-the-korad-kel103-programmable-load/?all

type ExperimentalUdpClient (endpoint:IPEndPoint) =
    let encoding = Encoding.ASCII
    let udpClient = new UdpClient(endpoint.Port)
    do udpClient.Client.ReceiveTimeout <- 500
    let local = IPEndPoint(IPAddress.Any, endpoint.Port)
    do udpClient.Connect(endpoint.Address, endpoint.Port)

    let read () = 
        try
            udpClient.Receive(ref local) |> encoding.GetString |> Some
        with 
            | :? SocketException -> None

    let readmany () = 
        Seq.initInfinite (fun _ -> read ()) 
        |> Seq.takeWhile Option.isSome 
        |> Seq.choose id 

    let send cmd = 
        let sendBytes = encoding.GetBytes(cmd:string)
        printfn "sending: %s" (cmd.Trim()) 
        udpClient.Send(sendBytes, sendBytes.Length) |> ignore

    member _.Raw cmd =
        send cmd
        let raw = readmany ()
        let result = raw |> Seq.toList
        String.Join("", result).Trim()

    member _.Query cmd =
        if canQuery cmd |> not then failwith "Cant query"
        let definition = toDef cmd
        send definition.Query
        readmany () 
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

