module Fidi.File.Smf

open System
open System.IO
open Microsoft.FSharp.Core.LanguagePrimitives

let read16 (r: BinaryReader) =
    uint16 (r.ReadByte()) <<< 8
    ||| uint16 (r.ReadByte())

let read24 (r: BinaryReader) =
    uint32 (r.ReadByte ()) <<< 16
    ||| uint32 (r.ReadByte ()) <<< 8
    ||| uint32 (r.ReadByte ())

let read32 (r: BinaryReader) =
    uint32 (r.ReadByte ()) <<< 24
    ||| uint32 (r.ReadByte ()) <<< 16
    ||| uint32 (r.ReadByte ()) <<< 8
    ||| uint32 (r.ReadByte ())

let rec readVar (r: BinaryReader, prevValue: uint32) =
    let b = r.ReadByte ()

    match b &&& 0x80uy with
    | 0x80uy -> readVar ( r, prevValue + uint32 (b &&& 0x7Fuy) <<< 7 )
    | _ -> prevValue + uint32 b

let readData (r: BinaryReader, length: uint32) =
    r.ReadBytes (int length)

let readString (r: BinaryReader, length: uint32) =
    // TODO: Try to guess other character encodings.
    Text.Encoding.ASCII.GetString (readData (r, length))

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
    | MidiEvent  of Channel: byte * MidiEvent // TODO: Channel type?
    | MetaEvent  of Length: uint32 * MetaEvent
    | SysExEvent of Length: uint32 * Type: EventType * Data: byte[]

type Event = {
    Delta    : uint32
    Status   : byte
    EventData: EventData
}

type Track = {
    Events: Event list
}

type ChunkData =
    | Header of Header
    | Track  of Track

type Magic =
    | Header = 1684558925 // MThd
    | Track  = 1802654797 // MTrk

type Chunk = {
    Magic : Magic
    Length: uint32
    Data  : ChunkData
}

type File = {
    Header: Chunk
    Tracks: Chunk list
}

let ParseDivision (d: uint16) =
    match d &&& 0x8000us with
    | 0x8000us -> Division.SmpteFramesPerSecond ( byte (d &&& 0x00FFus), byte ((d &&& 0x7F00us) >>> 8) )
    | _ -> Division.TicksPerQuarterNote d

let LoadHeader (r: BinaryReader) =
    let format : Format = EnumOfValue ( read16 r )
    let trackcount = read16 r
    let division = ParseDivision ( read16 r )

    ChunkData.Header {
        Format = format
        TrackCount = trackcount
        Division = division
    }

let ParseMetaEvent (r: BinaryReader) =
    let t : MetaEventType = EnumOfValue ( r.ReadByte () )
    let l = readVar (r, 0ul) // Length

    // TODO: Check if length matches fixed-length events.
    match t with
    | MetaEventType.SequenceNumber    -> l, MetaEvent.SequenceNumber ( read16 r )
    | MetaEventType.Text              -> l, MetaEvent.Text ( readString (r, l) )
    | MetaEventType.Copyright         -> l, MetaEvent.Copyright ( readString (r, l) )
    | MetaEventType.SequenceName      -> l, MetaEvent.SequenceName ( readString (r, l) )
    | MetaEventType.InstrumentName    -> l, MetaEvent.InstrumentName ( readString (r, l) )
    | MetaEventType.Lyric             -> l, MetaEvent.Lyric ( readString (r, l))
    | MetaEventType.Marker            -> l, MetaEvent.Marker ( readString (r, l))
    | MetaEventType.CuePoint          -> l, MetaEvent.CuePoint ( readString (r, l))
    | MetaEventType.ProgramName       -> l, MetaEvent.ProgramName ( readString (r, l))
    | MetaEventType.DeviceName        -> l, MetaEvent.DeviceName ( readString (r, l))
    | MetaEventType.MidiChannelPrefix -> l, MetaEvent.MidiChannelPrefix ( r.ReadByte ())
    | MetaEventType.MidiPort          -> l, MetaEvent.MidiPort ( r.ReadByte ())
    | MetaEventType.EndOfTrack        -> l, MetaEvent.EndOfTrack
    | MetaEventType.Tempo             -> l, MetaEvent.Tempo ( read24 r )
    | MetaEventType.SmtpeOffset       -> l, MetaEvent.SmtpeOffset ( r.ReadByte (), r.ReadByte (), r.ReadByte (), r.ReadByte (), r.ReadByte () )
    | MetaEventType.TimeSignature     -> l, MetaEvent.TimeSignature ( r.ReadByte (), r.ReadByte (), r.ReadByte (), r.ReadByte () )
    | MetaEventType.KeySignature      -> l, MetaEvent.KeySignature ( r.ReadSByte (), r.ReadByte () )
    | MetaEventType.SequencerEvent    -> l, MetaEvent.SequencerEvent ( readData (r, l) )
    | _ -> failwith (sprintf "Unknown meta event type 0x%X" (EnumToValue t))

let ParseMidiEvent (r: BinaryReader, status: byte) =
    let t : MidiMessageType = EnumOfValue ( (status &&& 0xF0uy) >>> 4 )
    let c = status &&& 0x0Fuy // MIDI channel

    // TODO: Check invalid channel values.

    match t with
    | MidiMessageType.NoteOff          -> c, MidiEvent.NoteOff ( r.ReadByte (), r.ReadByte ())
    | MidiMessageType.NoteOn           -> c, MidiEvent.NoteOn ( r.ReadByte (), r.ReadByte ())
    | MidiMessageType.KeyPressure      -> c, MidiEvent.KeyPressure ( r.ReadByte (), r.ReadByte ())
    | MidiMessageType.ControllerChange -> c, MidiEvent.ControllerChange ( r.ReadByte (), r.ReadByte ())
    | MidiMessageType.ProgramChange    -> c, MidiEvent.ProgramChange ( r.ReadByte () )
    | MidiMessageType.ChannelPressure  -> c, MidiEvent.ChannelPressure ( r.ReadByte ())
    | MidiMessageType.PitchBend        -> c, MidiEvent.PitchBend ( r.ReadByte (), r.ReadByte ())
    | _ -> failwith (sprintf "Unknown MIDI message type 0x%X" (EnumToValue t))

let ParseSysExEvent (r: BinaryReader, eventType: EventType) =
    let length = readVar (r, 0ul)
    EventData.SysExEvent (length, eventType, readData (r, length))

let ParseEvent (r: BinaryReader, prevStatus: byte) =
    let delta = readVar (r, 0ul)
    // Running status
    let status =
        match r.PeekChar () &&& 0x80 with
        | 0x80 -> r.ReadByte ()
        | _ -> prevStatus

    let et : EventType = EnumOfValue status

    let data =
        match et with
        | EventType.Meta ->
            EventData.MetaEvent ( ParseMetaEvent r )
        | EventType.SysExSimple | EventType.SysExRaw ->
            ParseSysExEvent ( r, et )
        | x when x > EventType.StatusFlag ->
            failwith (sprintf "Unknown event type 0x%X" status)
        | _ -> EventData.MidiEvent ( ParseMidiEvent (r, status) )

    status, {
        Delta = delta
        Status = status
        EventData = data
    }

let LoadTrack (r: BinaryReader) =
    // TODO: Resolve this hot mess.
    let status', event' = ParseEvent (r, 0uy)
    let mutable status = status'
    let mutable event = event'

    ChunkData.Track {
        Events = [
            yield event
            while (match event.EventData with | EventData.MetaEvent (c, EndOfTrack) -> false | _ -> true)
                do
                    let status', event' = ParseEvent (r, status)
                    status <- status'
                    event <- event'
                    yield event
        ]
    }

let LoadChunk (r: BinaryReader) =
    let magic : Magic = EnumOfValue ( r.ReadInt32 () )
    let length = read32 r

    // TODO: Check if length matches content.

    {
        Magic = magic
        Length = length
        Data = match magic with
                | Magic.Header -> LoadHeader r
                | Magic.Track  -> LoadTrack r
                | _ -> failwith (sprintf "Unknown magic bytes %A" magic )
    }

let LoadFile (r: BinaryReader) =
    let header = LoadChunk r
    let trackCount = match header.Data with | ChunkData.Header h -> h.TrackCount | _ -> failwith "No header"

    {
        Header = header
        Tracks = [
            for track in [1..int trackCount] do
                // TODO: Check if chunk is track.
                yield LoadChunk r
        ]
    }