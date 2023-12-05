open Parser
open Evaluator

let usage() = 
    printfn("Usage:")
    printfn("   dotnet run <file.rpg>")

[<EntryPoint>]
let main args =
    let file = System.IO.File.ReadAllLines args[0] |> String.concat ""
    let p = parse file

    match p with
    | Some ast -> 
        evalParagraph ast Map.empty
        0
    | None -> 
        printfn("Invalid program.")
        usage()
        1

