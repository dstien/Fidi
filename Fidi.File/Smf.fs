module Fidi.File.Smf

open System
open System.IO
open Microsoft.FSharp.Core.LanguagePrimitives

let read16 (r: BinaryReader) =
    uint16 (r.ReadByte()) <<< 8
    ||| uint16 (r.ReadByte())

let read24 (r: BinaryReader) =
    uint32 (r.ReadByte()) <<< 16
    ||| uint32 (r.ReadByte()) <<< 8
    ||| uint32 (r.ReadByte())

let read32 (r: BinaryReader) =
    uint32 (r.ReadByte()) <<< 24
    ||| uint32 (r.ReadByte()) <<< 16
    ||| uint32 (r.ReadByte()) <<< 8
    ||| uint32 (r.ReadByte())

let rec readVar (r: BinaryReader, prevValue: uint32) =
    let b = r.ReadByte()

    match b &&& 0x80uy with
    | 0x80uy -> readVar (r, prevValue + uint32 (b &&& 0x7Fuy) <<< 7)
    | _ -> prevValue + uint32 b

let readData (r: BinaryReader, length: uint32) =
    r.ReadBytes (int length)

let readString (r: BinaryReader, length: uint32) =
    // TODO: Try to guess other character encodings.
    Text.Encoding.ASCII.GetString(readData (r, length))

// Header
type Format =
    | Format0 = 0us
    | Format1 = 1us
    | Format2 = 2us

type Division =
    | TicksPerQuarterNote  of uint16
    | SmpteFramesPerSecond of DeltaPerFrame: byte * FramesPerSecond: byte

type Header = {
    Format    : Format
    TrackCount: uint16
    Division  : Division
}

// Track
type MidiMessageType =
    | NoteOff          = 0x8uy
    | NoteOn           = 0x9uy
    | KeyPressure      = 0xAuy
    | ControllerChange = 0xBuy
    | ProgramChange    = 0xCuy
    | ChannelPressure  = 0xDuy
    | PitchBend        = 0xEuy

type MidiEvent =
    | NoteOff          of Key: byte * Velocity: byte // TODO: Note type?
    | NoteOn           of Key: byte * Velocity: byte
    | KeyPressure      of Key: byte * Pressure: byte
    | ControllerChange of Number: byte * Value: byte // TODO: Controller types?
    | ProgramChange    of Program: byte // TODO: Program type?
    | ChannelPressure  of Pressure: byte
    | PitchBend        of LSB: byte * MSB: byte

type MetaEventType =
    | SequenceNumber    = 0x00uy
    | Text              = 0x01uy
    | Copyright         = 0x02uy
    | SequenceName      = 0x03uy
    | InstrumentName    = 0x04uy
    | Lyric             = 0x05uy
    | Marker            = 0x06uy
    | CuePoint          = 0x07uy
    | ProgramName       = 0x08uy
    | DeviceName        = 0x09uy
    | MidiChannelPrefix = 0x20uy
    | MidiPort          = 0x21uy
    | EndOfTrack        = 0x2Fuy
    | Tempo             = 0x51uy
    | SmtpeOffset       = 0x54uy
    | TimeSignature     = 0x58uy
    | KeySignature      = 0x59uy
    | SequencerEvent    = 0x7Fuy

type MetaEvent =
    | SequenceNumber    of Number: uint16
    | Text              of Text: string
    | Copyright         of Text: string
    | SequenceName      of Text: string
    | InstrumentName    of Text: string
    | Lyric             of Text: string
    | Marker            of Text: string
    | CuePoint          of Text: string
    | ProgramName       of Text: string
    | DeviceName        of Text: string
    | MidiChannelPrefix of Channel: byte
    | MidiPort          of Port: byte
    | EndOfTrack
    | Tempo             of UsPerQuarterNote: uint32
    | SmtpeOffset       of Hours: byte * Minutes: byte * Seconds: byte * Frames: byte * FractionalFrame: byte
    | TimeSignature     of Numerator: byte * Denominator: byte * ClocksPerClick: byte * DemisemiquaversPer24Clocks: byte
    | KeySignature      of FlatsOrSharps: sbyte * MajorOrMinor: byte
    | SequencerEvent    of Data: byte[]

type EventType =
    | StatusFlag  = 0x80uy
    | SysExSimple = 0xF0uy
    | SysExRaw    = 0xF7uy
    | Meta        = 0xFFuy

type EventData =
    | MidiEvent  of Channel: byte * MidiEvent // TODO: Channel type? Rename to ChannelEvent
    | MetaEvent  of Length: uint32 * MetaEvent
    | SysExEvent of Length: uint32 * Type: EventType * Data: byte[]

type Event = {
    Delta    : uint32
    Status   : byte // TODO: Remove
    EventData: EventData
}

type Track = {
    Events: Event list
}

type Magic =
    | Header = 1684558925 // MThd
    | Track  = 1802654797 // MTrk

type Chunk<'T> = {
    Magic : Magic
    Length: uint32
    Data  : 'T
}

type File = {
    Header: Chunk<Header>
    Tracks: Chunk<Track> list
}

let ParseDivision (d: uint16) =
    match d &&& 0x8000us with
    | 0x8000us -> Division.SmpteFramesPerSecond(byte (d &&& 0x00FFus), byte ((d &&& 0x7F00us) >>> 8))
    | _ -> Division.TicksPerQuarterNote d

let ParseMetaEvent (r: BinaryReader) =
    let type': MetaEventType = EnumOfValue(r.ReadByte())
    let length = readVar (r, 0ul)

    // TODO: Check if given length matches fixed-length events.
    let data =
        match type' with
        | MetaEventType.SequenceNumber    -> MetaEvent.SequenceNumber(read16 r)
        | MetaEventType.Text              -> MetaEvent.Text(readString (r, length))
        | MetaEventType.Copyright         -> MetaEvent.Copyright(readString (r, length))
        | MetaEventType.SequenceName      -> MetaEvent.SequenceName(readString (r, length))
        | MetaEventType.InstrumentName    -> MetaEvent.InstrumentName(readString (r, length))
        | MetaEventType.Lyric             -> MetaEvent.Lyric(readString (r, length))
        | MetaEventType.Marker            -> MetaEvent.Marker(readString (r, length))
        | MetaEventType.CuePoint          -> MetaEvent.CuePoint(readString (r, length))
        | MetaEventType.ProgramName       -> MetaEvent.ProgramName(readString (r, length))
        | MetaEventType.DeviceName        -> MetaEvent.DeviceName(readString (r, length))
        | MetaEventType.MidiChannelPrefix -> MetaEvent.MidiChannelPrefix(r.ReadByte())
        | MetaEventType.MidiPort          -> MetaEvent.MidiPort(r.ReadByte())
        | MetaEventType.EndOfTrack        -> MetaEvent.EndOfTrack
        | MetaEventType.Tempo             -> MetaEvent.Tempo(read24 r)
        | MetaEventType.SmtpeOffset       -> MetaEvent.SmtpeOffset(r.ReadByte(), r.ReadByte(), r.ReadByte(), r.ReadByte(), r.ReadByte())
        | MetaEventType.TimeSignature     -> MetaEvent.TimeSignature(r.ReadByte(), r.ReadByte(), r.ReadByte(), r.ReadByte())
        | MetaEventType.KeySignature      -> MetaEvent.KeySignature(r.ReadSByte(), r.ReadByte())
        | MetaEventType.SequencerEvent    -> MetaEvent.SequencerEvent(readData (r, length))
        | _ -> failwith (sprintf "Unknown meta event type 0x%X" (EnumToValue type'))

    EventData.MetaEvent(length, data)

// TODO: Rename to channel event?
let ParseMidiEvent (r: BinaryReader, runningStatus: byte option, currentStatus: byte) =
    let isStatus sb = sb >= 0x80uy && sb <= 0xEFuy

    let parse (r: BinaryReader, currentStatus, firstByte) =
        let channel = currentStatus &&& 0x0Fuy
        let type': MidiMessageType = EnumOfValue((currentStatus &&& 0xF0uy) >>> 4)

        let data =
            match type' with
            | MidiMessageType.NoteOff          -> MidiEvent.NoteOff(firstByte, r.ReadByte())
            | MidiMessageType.NoteOn           -> MidiEvent.NoteOn(firstByte, r.ReadByte())
            | MidiMessageType.KeyPressure      -> MidiEvent.KeyPressure(firstByte, r.ReadByte())
            | MidiMessageType.ControllerChange -> MidiEvent.ControllerChange(firstByte, r.ReadByte())
            | MidiMessageType.ProgramChange    -> MidiEvent.ProgramChange(firstByte)
            | MidiMessageType.ChannelPressure  -> MidiEvent.ChannelPressure(firstByte)
            | MidiMessageType.PitchBend        -> MidiEvent.PitchBend(firstByte, r.ReadByte())
            | _ -> failwith (sprintf "Unknown MIDI message type 0x%X" (EnumToValue type'))

        let nextStatus =
            if isStatus currentStatus then
                Some currentStatus
            else
                runningStatus

        EventData.MidiEvent(channel, data), nextStatus

    if isStatus currentStatus then
        parse (r, currentStatus, r.ReadByte())
    else
        match runningStatus with
        | Some runningStatus -> parse (r, runningStatus, currentStatus)
        | None -> failwith "Running status used without prior status byte"

let ParseSysExEvent (r: BinaryReader, eventType: EventType) =
    let length = readVar (r, 0ul)
    EventData.SysExEvent(length, eventType, readData (r, length))

let rec ParseEvents (r: BinaryReader, runningStatus: byte option, events: Event list) =
    // TODO: Check if exceeding length or reaching EOT with unparsed data
    let delta = readVar (r, 0ul)
    let currentStatus = r.ReadByte()
    let eventType: EventType = EnumOfValue currentStatus

    let (data, nextStatus) =
        match eventType with
        | EventType.Meta ->
            (ParseMetaEvent (r), runningStatus)
        | EventType.SysExSimple
        | EventType.SysExRaw ->
            (ParseSysExEvent (r, eventType), runningStatus)
        | _ ->
            ParseMidiEvent (r, runningStatus, currentStatus)

    let event = {
        Delta = delta
        Status = currentStatus
        EventData = data
    }

    match data with
    | EventData.MetaEvent(_, EndOfTrack) -> event :: events
    | _ -> ParseEvents(r, nextStatus, event :: events)

let ParseChunk<'T> (r: BinaryReader, expectedMagic: Magic, parseData: BinaryReader -> 'T) : Chunk<'T> =
    let magic: Magic = EnumOfValue(r.ReadInt32())
    if magic <> expectedMagic then
        failwith $"Expected {expectedMagic} chunk, got {magic}"

    // TODO: Check if length matches content.
    let length = read32 r

    {
        Magic = magic
        Length = length
        Data = parseData r
    }

let LoadFile (r: BinaryReader) =
    let parseHeader (r: BinaryReader) =
        {
            Format = EnumOfValue(read16 r)
            TrackCount = read16 r
            Division = ParseDivision(read16 r)
        }

    let parseTrack (r: BinaryReader) =
        {
            Events = ParseEvents(r, None, []) |> List.rev
        }

    let header = ParseChunk(r, Magic.Header, parseHeader)

    {
        Header = header
        Tracks = [
            for _ in [ 1 .. int header.Data.TrackCount ] do
                yield ParseChunk(r, Magic.Track, parseTrack)
        ]
    }
