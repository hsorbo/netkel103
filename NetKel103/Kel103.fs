﻿namespace NetKel103
open System

module Wire =
    open System.Text.RegularExpressions
    type ValueType =
        | Text
        | OnOff
        | NumericWithUnit
        | Numeric
        | Mode

    type CommandType =
        | Get of ValueType
        | Set
        | Both of ValueType

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
 
    let private doDef = function
        | ProductInformation        -> ("*IDN",          Get(Text))
        | StoreToUnit               -> ("*SAV",          Set) //<NR1> 1-100
        | RecallStorageUnit         -> ("*RCL",          Set) //<NR1> 1-100
        | SimulateExternalTrigger   -> ("*TRG",          Set) 
        | SystemBuzzer              -> (":SYST:BEEP",    Both(OnOff))
        | SystemBaud                -> (":SYST:BAUD",    Both(Numeric))
        | DeviceStatus              -> (":STAT",         Get(Text)) //The first byte is the buzzer status and the second byte is the baud rate; other bytes are to be determined.
        | Input                     -> (":INP",          Both(OnOff))
        | Voltage                   -> (":VOLT",         Both(NumericWithUnit)) //Changes Func Cv
        | VoltageMax                -> (":VOLT:UPP",     Both(NumericWithUnit))
        | VoltageMin                -> (":VOLT:LOW",     Both(NumericWithUnit))
        | Current                   -> (":CURR",         Both(NumericWithUnit)) //Changes Func Cc
        | CurrentMax                -> (":CURR:UPP",     Both(NumericWithUnit))
        | CurrentMin                -> (":CURR:LOW",     Both(NumericWithUnit))
        | Resistance                -> (":RES",          Both(NumericWithUnit)) //Changes Func Cr
        | ResistanceMax             -> (":RES:UPP",      Both(NumericWithUnit))
        | ResistanceMin             -> (":RES:LOW",      Both(NumericWithUnit))
        | Power                     -> (":POW",          Both(NumericWithUnit)) //Changes Func Cw
        | PowerMax                  -> (":POW:UPP",      Both(NumericWithUnit))
        | PowerMin                  -> (":POW:LOW",      Both(NumericWithUnit))
        | Function                  -> (":FUNC",         Both(Mode)) //Only can switch CV, CC, CR, CW. Can query CV, CC, CR, CW, that in continuous mode, pulse, flip, battery and all the other modes.
        | MeasureVoltage            -> (":MEAS:VOLT",    Get(NumericWithUnit))
        | MeasureAmp                -> (":MEAS:CURR",    Get(NumericWithUnit))
        | MeasurePower              -> (":MEAS:POW",     Get(NumericWithUnit))
        | OutputAllSteps            -> (":LIST",         Set) 
        | RecallList                -> (":RCL:LIST",     Both(Text)) //Recall the query unit before query, or an unknown condition occurs.
        | OutputAllStepsOcp         -> (":OCP",          Set)
        | RecallOcp                 -> (":RCL:OCP",      Both(Text))
        | OutputAllStepsOpp         -> (":OPP",          Set)
        | RecallOpp                 -> (":RCL:OPP",      Both(Text))
        | OutputAllStepsBattery     -> (":BATT",         Set)
        | RecallBattery             -> (":RCL:BATT",     Both(Text))
        | BatteryTime               -> (":BATT:TIM",     Get(NumericWithUnit))
        | BatteryCapacity           -> (":BATT:CAP",     Get(NumericWithUnit))
        | DynamicTestMode           -> (":DYN",          Both(Text))
        | SystemIpAddress           -> (":SYST:IPAD",    Both(Text))
        | SystemSubnetMask          -> (":SYST:SMASK",   Both(Text))
        | SystemGateway             -> (":SYST:GATE",    Both(Text))
        | SystemDhcp                -> (":SYST:DHCP",    Both(Numeric))
        | SystemMacAddress          -> (":SYST:MAC",     Get(Text))
        | SystemPort                -> (":SYST:PORT",    Both(Numeric))
        | SystemDeviceInfo          -> (":SYST:DEVINFO", Get(Text))
        | KeypadLock                -> (":SYST:LOCK",    Both(OnOff)) 
        | UndocumentedComp          -> (":COMP",         Both(Text)) 
        | UndocumentedExit          -> (":EXIT",         Both(Text)) //ON|OFF

    type OnOff = | On | Off
        with 
            static member Mapping = [("ON", On);("OFF", Off)] |> Map.ofList
            static member Parse s = Map.find s OnOff.Mapping
    
    type Measure = | A | V | W | Ohm
        with 
            static member Mapping = [("A", A);("V", V);("W", W);("OHM", Ohm)] |> Map.ofList
            static member Parse s = Map.find s Measure.Mapping
            static member ParseSpecial s = 
                let mtch = Regex("^(?<number>(\d|\.)+)(?<unit>A|V|W|OHM)$").Match(s)
                (mtch.Groups.["number"].Value |> float, mtch.Groups.["unit"].Value |> Measure.Parse)

    type Mode = | Cc | Cv | Cr | Cw
        with 
            static member Mapping = [("CC", Cc);("CV", Cv);("CR", Cr);("CW", Cw)] |> Map.ofList
            static member Parse s = Map.find s Mode.Mapping

    type CommandValue =
        | Nothing
        | StringValue of string
        | FloatWithUnitValue of number:float * unit:Measure
        | NumericValue of int
        | OnOffValue of OnOff
        | ModeValue of Mode

    let private createResponse cmdType (value:string) =
        let sanitized = value.Trim()
        let responseType = match cmdType with | Get t -> t | Both t -> t | _ -> Text
        if String.IsNullOrWhiteSpace(sanitized) then Nothing 
        else
            match responseType with
            | Text -> StringValue(sanitized)
            | NumericWithUnit -> sanitized |> Measure.ParseSpecial |> FloatWithUnitValue
            | OnOff -> OnOff.Parse(sanitized) |> OnOffValue
            | Mode -> Mode.Parse(sanitized) |> ModeValue
            | Numeric -> sanitized |> int |> NumericValue
    
    type CommandDefinition = { 
        Prefix:string
        Query:string
        Type:CommandType
        ResponseHandler:string->CommandValue
        HappyWithResponse:string->bool
        GenerateRaw:string->string
    }

    let toDef cmd =
        let prefix, cmdType = doDef cmd
        {
            Prefix = prefix
            Query = sprintf "%s?\n" prefix
            Type = cmdType
            ResponseHandler = (createResponse cmdType)
            HappyWithResponse = (fun x ->
                match cmd with
                | SystemDeviceInfo -> x |> Seq.filter (fun x -> x = '\n') |> Seq.length > 6
                | _ -> x.Contains("\n"))
            GenerateRaw = (fun x -> sprintf "%s %s\n" prefix x )
        }

    let canQuery cmd =
        match (cmd |> doDef |> snd) with
        | Set _ -> false
        | _ -> true
