open Parser
open Evaluator

let usage() = 
    printfn("Usage:")
    printfn("   dotnet run <file.rpg>")

[<EntryPoint>]
let main args =
    let file = System.IO.File.ReadAllLines args[0] |> String.concat ""
    //printfn "File contents: %s" file
    let p = parse file

    match p with
    | Some ast -> 
        //printfn "Successful parse... Starting evaluation..."
        let i = evalParagraph ast
        0
    | None -> 
        printfn("FAILED")
        usage()
        1

