open System

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        let filename = argv.[0]

        use reader =
            new IO.BinaryReader(IO.File.Open(filename, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read))

        let smf = Fidi.File.Smf.LoadFile reader
        printfn "%A" smf
        0
    | _ ->
        printfn "Usage: Fidi.FileTool song.mid"
        1
