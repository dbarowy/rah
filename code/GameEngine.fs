module GameEngine

open AST

type mapEnv = Map<string, Room>
type charEnv = Map<string, Character>
type objEnv = Map<string, Object>

type GameState = { characters: charEnv;
                   rooms: mapEnv;
                   objects: objEnv;
                   turn: int;
                   mcL: string;
                   alive: bool; }

type Commands = 
| Attack
| Defend
| Abilities
| Flee

let gameOver = 
    printfn "You have lost!"

// Convert a map with 1 element to that element
let scml = 
    fun m ->
        if (Map.count m = 1) then
            m |> Map.toList |> List.head
        else
            failwith "More than one element"

(*
    finds the location of the player character
*)
let findCurrentRoom =
    fun (m: mapEnv) (mc: Character) ->
        let rec getCharList = 
            fun (ss: Sentence list) (charList: Character list) ->
                match ss with
                | [] -> charList
                | s::ss -> 
                    match s with
                    | Character c -> getCharList (ss) (c::charList)
                    | _-> getCharList (ss) (charList)

        m |> Map.filter (fun n r -> 
                (List.contains (mc) (getCharList r.objects []))
            ) 

let moveCharacter =
    fun (cr: Room) (nr: Room) (char: Character) (gs: GameState)->
        // remove character c from current room cr
        let objs = cr.objects
        let charLoc = objs |> List.findIndex (fun s ->
            match s with
            | Character c ->
                if c = char then 
                    true
                else
                    false
            | Object o ->
                false
        )

        let updatedCR = { name = cr.name;
                                descriptor = cr.descriptor;
                                objects = objs |> List.removeAt charLoc;
                                connections = cr.connections; }

        let cs: Sentence = Character char
        let updatedNR = { name = nr.name;
                                 descriptor = nr.descriptor;
                                 objects = cs::nr.objects
                                 connections = nr.connections; }
        
        let newMap = gs.rooms |> Map.add updatedCR.name updatedCR |> Map.add updatedNR.name updatedNR

        { characters = gs.characters;
          rooms = newMap; 
          objects = gs.objects;
          turn = gs.turn;
          mcL = nr.name;
          alive = gs.alive}


let rec playGame =  
    fun gs ->
        let map = gs.rooms
        let characters = gs.characters
        let objects = gs.objects
        let mc = characters |> Map.filter (fun n c -> c.typep = "Playable") |> scml |> snd

        let currentRoom = findCurrentRoom map mc |> scml |> snd
        let connections = currentRoom.connections

        if gs.alive = true then
            printfn "Current Room is: %A" currentRoom.name
            printfn "%A" currentRoom.descriptor
            printf ">"

            let input = System.Console.ReadLine()

            if input = "quit" then
                printfn "Goodbye! Thanks for playing."
                exit 0
            else if List.contains input (List.map (fun c -> fst c) connections) then // todo clean up functions
                let nr = connections |> List.filter (fun c -> fst c = input) |> List.head
                let nextRoom = map |> Map.filter (fun s r -> s = (snd nr)) |> scml |> snd

                playGame (moveCharacter currentRoom nextRoom mc gs)
            else
                printfn "I'm sorry, that is invalid input. Please try again." 
                playGame gs 
        else
            gameOver