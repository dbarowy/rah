open Parser
open Evaluator
open GameEngine

let usage() = 
    printfn("Usage:")
    printfn("   dotnet run <file.rpg>")

[<EntryPoint>]
let main args =
    let file = 
        if Array.length args <> 0 then 
            try
                Some (System.IO.File.ReadAllLines args[0] |> String.concat "")
            with
                | :? System.IO.FileNotFoundException -> 
                    printfn "Sorry, no valid file detected!"
                    exit 1
        else
            None

    let p = 
        match file with 
        | Some f -> parse f
        | None -> 
            printfn "Sorry, no valid file detected!"
            exit 1
            


    // TODO Exception handling in case file can't be found

    match p with
    | Some ast -> 
        evalParagraph ast Map.empty
        0
    | None -> 
        printfn("Invalid program.")
        usage()
        1

