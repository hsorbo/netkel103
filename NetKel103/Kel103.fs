namespace NetKel103

open System

module Wire =
    open System.Text.RegularExpressions

    type ValueType =
        | Text
        | OnOff
        | NumericWithUnit
        | Numeric
        | Mode
        | NoArg

    type CommandType =
        | Get of returns: ValueType
        | Set of arg: ValueType
        | Both of arg: ValueType * returns: ValueType

    module CommandType =
        let argType =
            function
            | Set x
            | Both (x, _) -> x |> Some
            | Get _ -> None

        let canQuery =
            function
            | Set _ -> false
            | _ -> true

        let canSet =
            function
            | Get _ -> false
            | _ -> true

    type Commands =
        | ProductInformation
        | StoreToUnit
        | RecallStorageUnit
        | SimulateExternalTrigger
        | SystemBuzzer
        | SystemBaud
        | DeviceStatus
        | Input
        | Voltage
        | VoltageMax
        | VoltageMin
        | Current
        | CurrentMax
        | CurrentMin
        | Resistance
        | ResistanceMax
        | ResistanceMin
        | Power
        | PowerMax
        | PowerMin
        | Function
        | MeasureVoltage
        | MeasureAmp
        | MeasurePower
        | OutputAllSteps
        | RecallList
        | OutputAllStepsOcp
        | RecallOcp
        | OutputAllStepsOpp
        | RecallOpp
        | OutputAllStepsBattery
        | RecallBattery
        | BatteryTime
        | BatteryCapacity
        | DynamicTestMode
        | SystemIpAddress
        | SystemSubnetMask
        | SystemGateway
        | SystemDhcp
        | SystemMacAddress
        | SystemPort
        | SystemDeviceInfo
        | KeypadLock
        | UndocumentedComp
        | UndocumentedExit

    type CommandInfo = { Command: Commands; Raw: string; Type: CommandType }

    let knownCommands =
        [ (ProductInformation, "*IDN", Get(Text))
          (StoreToUnit, "*SAV", Set(Numeric)) //<NR1> 1-100
          (RecallStorageUnit, "*RCL", Set(Numeric)) //<NR1> 1-100
          (SimulateExternalTrigger, "*TRG", Set(NoArg))
          (SystemBuzzer, ":SYST:BEEP", Both(OnOff, OnOff))
          (SystemBaud, ":SYST:BAUD", Both(Numeric, Numeric))
          (DeviceStatus, ":STAT", Get(Text)) //The first byte is the buzzer status and the second byte is the baud rate; other bytes are to be determined.
          (Input, ":INP", Both(OnOff, OnOff))
          (Voltage, ":VOLT", Both(NumericWithUnit, NumericWithUnit)) //Changes Func Cv
          (VoltageMax, ":VOLT:UPP", Both(NumericWithUnit, NumericWithUnit))
          (VoltageMin, ":VOLT:LOW", Both(NumericWithUnit, NumericWithUnit))
          (Current, ":CURR", Both(NumericWithUnit, NumericWithUnit)) //Changes Func Cc
          (CurrentMax, ":CURR:UPP", Both(NumericWithUnit, NumericWithUnit))
          (CurrentMin, ":CURR:LOW", Both(NumericWithUnit, NumericWithUnit))
          (Resistance, ":RES", Both(NumericWithUnit, NumericWithUnit)) //Changes Func Cr
          (ResistanceMax, ":RES:UPP", Both(NumericWithUnit, NumericWithUnit))
          (ResistanceMin, ":RES:LOW", Both(NumericWithUnit, NumericWithUnit))
          (Power, ":POW", Both(NumericWithUnit, NumericWithUnit)) //Changes Func Cw
          (PowerMax, ":POW:UPP", Both(NumericWithUnit, NumericWithUnit))
          (PowerMin, ":POW:LOW", Both(NumericWithUnit, NumericWithUnit))
          (Function, ":FUNC", Both(Mode, Mode)) //Only can switch CV, CC, CR, CW. Can query CV, CC, CR, CW, that in continuous mode, pulse, flip, battery and all the other modes.
          (MeasureVoltage, ":MEAS:VOLT", Get(NumericWithUnit))
          (MeasureAmp, ":MEAS:CURR", Get(NumericWithUnit))
          (MeasurePower, ":MEAS:POW", Get(NumericWithUnit))
          (OutputAllSteps, ":LIST", Set(Text))
          (RecallList, ":RCL:LIST", Both(Numeric, Text)) //Recall the query unit before query, or an unknown condition occurs.
          (OutputAllStepsOcp, ":OCP", Set(Text))
          (RecallOcp, ":RCL:OCP", Both(Numeric, Text))
          (OutputAllStepsOpp, ":OPP", Set(Text))
          (RecallOpp, ":RCL:OPP", Both(Numeric, Text))
          (OutputAllStepsBattery, ":BATT", Set(Text))
          (RecallBattery, ":RCL:BATT", Both(Numeric, Text))
          (BatteryTime, ":BATT:TIM", Get(NumericWithUnit))
          (BatteryCapacity, ":BATT:CAP", Get(NumericWithUnit))
          (DynamicTestMode, ":DYN", Both(Text, Text))
          (SystemIpAddress, ":SYST:IPAD", Both(Text, Text))
          (SystemSubnetMask, ":SYST:SMASK", Both(Text, Text))
          (SystemGateway, ":SYST:GATE", Both(Text, Text))
          (SystemDhcp, ":SYST:DHCP", Both(Numeric, Numeric))
          (SystemMacAddress, ":SYST:MAC", Get(Text))
          (SystemPort, ":SYST:PORT", Both(Numeric, Numeric))
          (SystemDeviceInfo, ":SYST:DEVINFO", Get(Text))
          (KeypadLock, ":SYST:LOCK", Both(OnOff, OnOff))
          (UndocumentedComp, ":COMP", Both(Text, Text))
          (UndocumentedExit, ":EXIT", Both(Text, Text)) ] //ON|OFF
        |> List.map (fun (cmd, raw, cmdtype) -> { Command = cmd; Raw = raw; Type = cmdtype })

    let toInfo (cmd: Commands) =
        knownCommands
        |> List.filter (fun x -> cmd = x.Command)
        |> List.exactlyOne

    type OnOff =
        | On
        | Off

    module OnOff =
        let fromString =
            function
            | "ON" -> On
            | "OFF" -> Off
            | _ -> failwith "Unknown value"

        let toString =
            function
            | On -> "ON"
            | Off -> "OFF"

    type Measure =
        | A
        | Ah
        | V
        | W
        | Ohm

    module Measure =
        let fromString =
            function
            | "A" -> A
            | "AH" -> Ah
            | "V" -> V
            | "W" -> W
            | "OHM" -> Ohm
            | _ -> failwith "unknown string"

        let toString =
            function
            | A -> "A"
            | Ah -> "AH"
            | V -> "V"
            | W -> "W"
            | Ohm -> "OHM"

        let parseSpecial s =
            let mtch =
                Regex("^(?<number>(\d|\.)+)(?<unit>A|V|W|OHM|AH)$")
                    .Match(s)

            (mtch.Groups.["number"].Value |> float, mtch.Groups.["unit"].Value |> fromString)

    type Mode =
        | Cc
        | Cv
        | Cr
        | Cw

    module Mode =
        let fromString =
            function
            | "CC" -> Cc
            | "CV" -> Cv
            | "CR" -> Cr
            | "CW" -> Cw
            | _ -> failwith "parse error"

        let toString =
            function
            | Cc -> "CC"
            | Cv -> "CV"
            | Cr -> "CR"
            | Cw -> "CW"

    type CommandValue =
        | Nothing
        | StringValue of string
        | FloatWithUnitValue of number: float * unit: Measure
        | NumericValue of int
        | OnOffValue of OnOff
        | ModeValue of Mode

    module CommandValue =
        let fromString (value: string) valueType =
            let sanitized = value.Trim()

            if String.IsNullOrWhiteSpace(sanitized) then
                Nothing
            else
                match valueType with
                | Text -> StringValue(sanitized)
                | NumericWithUnit ->
                    sanitized
                    |> Measure.parseSpecial
                    |> FloatWithUnitValue
                | OnOff -> OnOff.fromString sanitized |> OnOffValue
                | Mode -> Mode.fromString sanitized |> ModeValue
                | Numeric -> sanitized |> int |> NumericValue
                | NoArg -> Nothing

        let toString =
            function
            | StringValue (x) -> x
            | FloatWithUnitValue (n, v) -> sprintf "%f%s" n (v |> Measure.toString)
            | OnOffValue x -> x |> OnOff.toString
            | ModeValue x -> x |> Mode.toString
            | Nothing -> ""
            | NumericValue x -> sprintf "%i" x

        let toPrimitives =
            function
            | FloatWithUnitValue (x, d) -> box x
            | OnOffValue x -> OnOff.toString x
            | StringValue x -> x
            | Nothing -> null
            | ModeValue m -> Mode.toString m
            | NumericValue x -> x

    let private createResponse cmdType (value: string) =
        let responseType =
            match cmdType with
            | Get t -> t
            | Both (arg, ret) -> ret
            | _ -> Text

        CommandValue.fromString value responseType

    type CommandDefinition =
        { Prefix: string
          Query: string
          Type: CommandType
          ResponseHandler: string -> CommandValue
          HappyWithResponse: string -> bool
          GenerateRaw: string -> string }

    let toDef (info: CommandInfo) =
        { Prefix = info.Raw
          Query = sprintf "%s?\n" info.Raw
          Type = info.Type
          ResponseHandler = (createResponse info.Type)
          HappyWithResponse =
            (fun x ->
                match info.Command with
                | SystemDeviceInfo -> x |> Seq.filter (fun x -> x = '\n') |> Seq.length > 6
                | _ -> x.Contains("\n"))
          GenerateRaw = (fun x -> sprintf "%s %s\n" info.Raw x) }
