namespace NetKel103

open System
open System.Net
open System.Text
open System.Net.Sockets
open NetKel103.Wire
open NetKel103.TextUtils

module NetworkUtils =
    let read (udpClient: UdpClient) =
        let mutable local = IPEndPoint(0, 0)

        try
            //Some(local, udpClient.Receive(&local))
            udpClient.Receive(&local) |> Some
        with
        //Error 60 macos, error 110 linux
        //| :? SocketException as ex when ex.ErrorCode = 60 -> None
        | :? SocketException -> None

    let readMany udpClient =
        Seq.initInfinite (fun _ -> read udpClient)
        |> Seq.takeWhile Option.isSome
        |> Seq.choose id

    let searchNetkel () =
        seq {
            let encoding = Encoding.ASCII
            let dst = IPEndPoint(IPAddress.Broadcast, 18191)
            use udpClient = new UdpClient()

            udpClient.Send(encoding.GetBytes "find_ka000", dst)
            |> ignore

            udpClient.Client.ReceiveTimeout <- 1000

            let parseResponse s =
                RegularExpressions.Regex("([0-9|\.]+)\s*?([0-9|a-f|-]+)\s*?(\d+)")
                |> rxmatch s
                |> Option.map (fun x -> x.Groups)
                |> Option.map (fun m ->
                    {| Ip = IPAddress.Parse(m.[1].Value); Mac = m.[2].Value; Port = (int m.[3].Value) |})

            yield!
                udpClient
                |> readMany
                |> Seq.map encoding.GetString
                |> Seq.map parseResponse
                |> Seq.choose id
        }

type ExperimentalUdpClient(endpoint: IPEndPoint) =
    let encoding = Encoding.ASCII
    let udpClient = new UdpClient(endpoint.Port)
    do udpClient.Client.ReceiveTimeout <- 500
    //let local = IPEndPoint(IPAddress.Any, endpoint.Port)
    //do udpClient.Connect(endpoint.Address, endpoint.Port)

    let send cmd =
        let sendBytes = encoding.GetBytes(cmd: string)
        //printfn "sending: %s" (cmd.Trim())
        udpClient.Send(sendBytes, sendBytes.Length, endpoint)
        |> ignore

    member _.Raw cmd =
        send cmd

        udpClient
        |> NetworkUtils.readMany
        |> Seq.map encoding.GetString
        |> Seq.toList
        |> (join "")
        |> trim

    member _.Set command (value: CommandValue) =
        let info = toInfo command
        send (sprintf "%s %s\n" info.Raw (CommandValue.toString value))


    member _.Query cmd =
        let info = toInfo cmd
        let definition = toDef info

        if CommandType.canQuery info.Type |> not then
            failwith "Cant query"

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

    interface IDisposable with
        member _.Dispose() = udpClient.Dispose()
