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

(*
    convert a map with 1 element to that element
*)
let scml = 
    fun (m: Map<'a,'b>) ->
        if (Map.count m = 1) then
            Some (m |> Map.toList |> List.head)
        else
            None

(*
    Gets a list of the characters a room's objects
*)
let rec getCharList = 
            fun (ss: Sentence list) (charList: Character list) ->
                match ss with
                | [] -> 
                    charList
                | s::ss -> 
                    match s with
                    | Character c -> getCharList (ss) (c::charList)
                    | _-> getCharList (ss) (charList)

(*
    Gets a list of the objects in a room's objects
*)
let rec getObjList = 
            fun (ss: Sentence list) (objList: Object list) ->
                match ss with
                | [] -> 
                    objList
                | s::ss -> 
                    match s with
                    | Object o -> getObjList (ss) (o::objList)
                    | _ -> getObjList (ss) (objList)

(*
    finds the location of the player character
*)
let findCurrentRoom =
    fun (m: mapEnv) (c: Character) ->
        m |> Map.filter (fun n r -> 
                (List.contains (c) (getCharList r.objects []))
        ) 

(*
    updates map with a new room state
*)
let newMap =
    fun updatedCr gs->
        let newMap = gs.rooms |> Map.add updatedCr.name updatedCr

        { characters = gs.characters;
          rooms = newMap; 
          objects = gs.objects;
          turn = gs.turn;
          mcL = updatedCr.name;
          alive = gs.alive}

(*
    modifies the gamestate so that the given character appears in a different room
*)
let moveCharacter =
    fun (cr: Room) (nr: Room) (char: Character) (gs: GameState) ->
        // remove character c from current room cr
        let objs = cr.objects
        let charLoc = 
            objs |> List.findIndex (fun s ->
            match s with
            | Character c ->
                if c = char then 
                    true
                else
                    false
            | Object o ->
                false
            )

        let updatedCr = { name = cr.name;
                                descriptor = cr.descriptor;
                                objects = objs |> List.removeAt charLoc;
                                connections = cr.connections; }

        let cs = Character char
        let updatedNr = { name = nr.name;
                                 descriptor = nr.descriptor;
                                 objects = cs::nr.objects
                                 connections = nr.connections; }

        newMap updatedNr (newMap updatedCr gs)
        
(*
    updates MapEnv to reflect character status
*)
let updatedCharsRoom =
    fun (cr: Room) (char: Character) (gs: GameState) ->
        let objs = cr.objects
        let charLoc = 
            objs |> List.findIndex (fun s ->
            match s with
            | Character c ->
                if c.name = char.name then 
                    true
                else
                    false
            | Object o ->
                false
            )

        let updatedCr = { name = cr.name; 
                                descriptor = cr.descriptor;
                                objects = Character char::(objs |> List.removeAt charLoc);
                                connections = cr.connections}
        
        newMap updatedCr gs

(*
    main game loop, plays the game until the game is over
*)
let rec playGame =
    fun gs ->
        let map = gs.rooms
        let characters = gs.characters
        let mc = 
            match characters |> Map.filter (fun n c -> c.typep = "Playable") |> scml with
            | Some m -> snd m
            | None -> 
                printfn "Error! Playable character not found."
                exit 1


        let currentRoom = 
            match findCurrentRoom map mc |> scml with
            | Some m -> snd m
            | None ->
                printfn "Error! Room not found."
                exit 1
        let connections = currentRoom.connections
        let objects = currentRoom.objects

        let containsConnection = 
            fun i ->
                List.contains i (List.map (fun (c: string * string) -> (fst c).ToLower() ) connections)

        let containsObjects =
            fun i ->
                List.contains i (List.map (fun (s) ->
                    match s with
                    | Object o -> o.name.ToLower()
                    | Character c -> c.name.ToLower()) objects
                )

        let containsInteraction = 
            fun (i: string) ->
                let os = 
                    (List.filter (fun s -> (
                        match s with
                        | Object o -> 
                            if (List.contains i (List.map (fun (i: Interaction) -> i.name.ToLower()) o.interactions)) then true else false
                        | _ -> false
                        )
                    ) objects)

                let o =
                    if os.Length = 1 then Some (List.head os) else None
                
                match o with
                | Some o ->
                    let obj = 
                        match o with
                        | Object b -> b
                        | _ -> { name = ""; descriptor = "placeholder"; interactions = []}

                    let interactions = obj.interactions

                    List.contains i (List.map (fun (i: Interaction) -> i.name.ToLower()) interactions)
                | None -> false

        (*
            conducts combat between two given characters
        *)
        let rec fight =
            fun mc e gs ->
                let variance = (System.Random().Next(-20, 20))/10

                let playerTurn input = 
                    match input with
                    | "attack" | "a" -> 
                        printfn "You attack!"
                        let damage = mc.stats.atk + variance + - (e.stats.def*3)/4
                        printfn "You deal %d damage!" damage
                        let updatedS = { hp = e.stats.hp - damage;
                                                mp = e.stats.mp;
                                                atk = e.stats.atk;
                                                def = e.stats.def;
                                                matk = e.stats.matk;
                                                mdef = e.stats.mdef;
                                                spd = e.stats.spd; }
                        let updatedE = { typep = e.typep;
                                                    name = e.name;
                                                    stats = updatedS;
                                                    abilities = e.abilities; }
                        let ngs = updatedCharsRoom currentRoom updatedE gs
                        fight mc updatedE ({ characters = ngs.characters;
                                             rooms = ngs.rooms;
                                             objects = ngs.objects;
                                             turn = ngs.turn + 1;
                                             mcL = ngs.mcL;
                                             alive = ngs.alive })

                    | "guard" | "g" -> 
                        printfn "You guard!"
                        let guard = mc.stats.def / 10 + variance
                        printfn "You heal %d health!" guard
                        let updatedS = { hp = mc.stats.hp + guard;
                                                mp = mc.stats.mp;
                                                atk = mc.stats.atk;
                                                def = mc.stats.def;
                                                matk = mc.stats.matk;
                                                mdef = mc.stats.mdef;
                                                spd = mc.stats.spd; }
                        let updatedMC = { typep = mc.typep;
                                                    name = mc.name;
                                                    stats = updatedS;
                                                    abilities = mc.abilities; }
                        let ngs = updatedCharsRoom currentRoom updatedMC gs
                        fight updatedMC e ({ characters = ngs.characters;
                                             rooms = ngs.rooms;
                                             objects = ngs.objects;
                                             turn = ngs.turn + 1;
                                             mcL = ngs.mcL;
                                             alive = ngs.alive })

                    | "use ability" | "u" -> 
                        printfn "You use an ability!"
                        fight mc e gs

                    | "diagnose" | "d" ->
                        printfn "Your health is: %d" mc.stats.hp
                        printfn "%s's health is: %d" e.name e.stats.hp
                        fight mc e gs

                    | "quit" | "q" -> 
                        printfn "Thanks for playing!"
                        exit 0

                    | "help" | "h" -> 
                        printfn "Here is a list of valid commands while fighting:"
                        printfn "   'attack' or 'a': attacks the enemy."
                        printfn "   'guard' or 'g': defends this turn."
                        printfn "   'use ability' or 'u': uses an ability."
                        printfn "   'diagnose' or 'd': gives a report of the player's physical condition."
                        printfn "   'quit' or 'q': exits the game."
                        fight mc e gs
                        
                    | _ -> 
                        printfn "I'm sorry, that is invalid input. If you need help with valid commands, type 'help'. Please try again."
                        fight mc e gs
                (* 
                    END PLAYER TURN
                *)

                let enemyTurn gs =
                    let r = System.Random()
                    let rnum = r.Next 10

                    if rnum < 5 then 
                        // attack
                        printfn "The enemy attacks!"
                        let damage = e.stats.atk + variance + - (mc.stats.def*3)/4
                        printfn "%s deals %d damage to you!" e.name damage
                        let updatedS = { hp = mc.stats.hp - (e.stats.atk - (mc.stats.def * 3)/4);
                                                mp = mc.stats.mp;
                                                atk = mc.stats.atk;
                                                def = mc.stats.def;
                                                matk = mc.stats.matk;
                                                mdef = mc.stats.mdef;
                                                spd = mc.stats.spd; }
                        let updatedMC = { typep = mc.typep;
                                                    name = mc.name;
                                                    stats = updatedS;
                                                    abilities = mc.abilities; }
                        let ngs = updatedCharsRoom currentRoom updatedMC gs
                        fight updatedMC e ({ characters = ngs.characters;
                                             rooms = ngs.rooms;
                                             objects = ngs.objects;
                                             turn = ngs.turn + 1;
                                             mcL = ngs.mcL;
                                             alive = ngs.alive })

                    else if rnum < 7 then
                        // ability
                        printfn "The enemy uses an ability!"
                        fight mc e gs

                    else 
                        // guard
                        printfn "The enemy guards!"
                        let guard = e.stats.def / 10 + variance
                        printfn "%s heals %d health!" e.name guard
                        let updatedS = { hp = e.stats.hp + 3;
                                                mp = e.stats.mp;
                                                atk = e.stats.atk;
                                                def = e.stats.def;
                                                matk = e.stats.matk;
                                                mdef = e.stats.mdef;
                                                spd = e.stats.spd; }
                        let updatedE = { typep = e.typep;
                                                    name = e.name;
                                                    stats = updatedS;
                                                    abilities = e.abilities; }
                        let ngs = updatedCharsRoom currentRoom updatedE gs
                        fight mc updatedE ({ characters = ngs.characters;
                                             rooms = ngs.rooms;
                                             objects = ngs.objects;
                                             turn = ngs.turn + 1;
                                             mcL = ngs.mcL;
                                             alive = ngs.alive })
                (*
                    END ENEMY TURN
                *)
            
                if mc.stats.hp > 0 && e.stats.hp > 0 then
                    printfn "\nTurn: %d" (gs.turn + 1)
                    let turnIsEven = if gs.turn % 2 = 0 then true else false

                    if gs.turn = 0 && mc.stats.spd >= e.stats.spd then
                        printfn "You get ready to fight. Attack, guard, or use an ability?"
                        printf ">"

                        let i = System.Console.ReadLine()
                        playerTurn (i.ToLower())

                    else if gs.turn = 0 && mc.stats.spd <= e.stats.spd then
                        printfn "\nThe opponent is faster."
                        enemyTurn gs

                    else if turnIsEven then
                        printfn "Attack, guard, or use an ability?"
                        printf ">"
                        let i = System.Console.ReadLine()
                        playerTurn (i.ToLower())
                    
                    else
                        enemyTurn gs

                else
                    if mc.stats.hp <= 0 then 
                        printfn "You have died and lost!"
                        exit 0

                    else if e.stats.hp <= 0 then
                        printfn "You defeat the enemy."
                        let currentRoom = 
                            match findCurrentRoom gs.rooms e |> scml with
                            | Some m -> snd m
                            | None ->
                                printfn "Error! Room doesn't exist."
                                exit 1
                        let updatedObjects = 
                            List.removeAt (List.findIndex (fun s ->
                                match s with
                                | Character c ->
                                    if c = e then 
                                        true
                                    else
                                        false
                                | Object o ->
                                    false
                                ) currentRoom.objects ) currentRoom.objects
                            
                        let updatedRoom = { name = currentRoom.name; 
                                                   descriptor = currentRoom.descriptor;
                                                   objects = updatedObjects; 
                                                   connections = connections; }
                                                   
                        playGame ({ characters = gs.characters |> Map.remove e.name;
                                    rooms = gs.rooms |> Map.add updatedRoom.name updatedRoom;
                                    objects = gs.objects;
                                    turn = 0;
                                    mcL = gs.mcL;
                                    alive = gs.alive})

        (*
            END FIGHT
        *)
        if gs.alive = true then
            printfn "\n%A" currentRoom.name
            printfn "%A" currentRoom.descriptor
            printf ">"

            let i = System.Console.ReadLine()
            let input = i.ToLower()
            let words = input.Split(" ")

            match input with
            | "quit" | "q" ->
                printfn "Goodbye! Thanks for playing."
                exit 0

            | "help" | "h" ->
                printfn "Here is a list of valid commands: "
                printfn "   'help' or 'h': provides a list of useful inputs. "
                printfn "   'quit' or 'q': exits the game. "
                printfn "   'look' or 'l': produces a description of the current location."
                printfn "   'diagnose' or 'd': gives a report of the player's physical condition."
                printfn "   'move direction' or 'go 'direction': if the direction is applicable, moves to the location in that direction."
                printfn "   'fight' or 'attack': initiates combat with a character in the same room."
                printfn "   '<object name>: lists possible interactions with the object"
                printfn "   '<interaction name>: executes the interaction"
                playGame gs
                
            | "look" | "l" ->
                playGame gs

            | "diagnose" | "d" ->
                printfn "Current health is: %d" mc.stats.hp
                playGame gs 

            | _ when (containsConnection words[0]) || (words[0] = "move" && containsConnection words[1]) || (words[0] = "go" && containsConnection words[1]) ->
                let direction =
                    match words with
                    | _ when containsConnection words[0] -> words[0]
                    | _ -> words[1]

                let nr = connections |> List.filter (fun c -> (fst c).ToLower() = direction) |> List.head
                let nextRoom = 
                    match map |> Map.filter (fun s r -> s = (snd nr)) |> scml with
                    | Some m -> snd m
                    | None ->
                        printfn "Error! Room doesn't exist"
                        exit 1
                playGame (moveCharacter currentRoom nextRoom mc gs)

            | "fight" | "attack" | "f" | "a" ->
                let n = characters |> Map.remove mc.name |> (Map.filter (fun n c -> (
                    match (findCurrentRoom map c) |> scml with
                    | Some m -> (snd) m = currentRoom
                    | None ->
                        printfn "Error! Room doesn't exist."
                        exit 1
                        ) 
                    )) 
                let nchar = 
                    if n.Count <> 0 then 
                        (n |> Map.toList |> List.head |> snd)
                    else 
                        printfn "There is no one to fight here."
                        mc


                if nchar = mc then 
                    printfn "There is no one to fight here."
                    playGame gs
                else
                    fight mc nchar gs

            | _ when (containsObjects input) ->
                let o = 
                    objects |> List.filter (fun s -> (
                        match s with
                        | Object o -> 
                            if o.name.ToLower() = input then true else false
                        | _ -> false
                        )
                    ) |> List.head

                let obj = 
                    match o with
                    | Object b -> b
                    | _ -> { name = ""; descriptor = "placeholder"; interactions = []}

                printfn "%s: %s" obj.name obj.descriptor
                
                let interactions = obj.interactions
                let iNames = (interactions |> List.map (fun i -> i.name))

                printfn "Possible Interactions: %A" (List.map (fun n -> n + "\n") iNames)

                playGame gs

            | _ when (containsInteraction input) ->
                let o = 
                    List.head (List.filter (fun s -> (
                        match s with
                        | Object o -> 
                            if (List.contains input (List.map (fun (i: Interaction) -> i.name.ToLower()) o.interactions)) then true else false
                        | _ -> false
                        )
                    ) objects)

                let obj = 
                    match o with
                    | Object b -> b
                    | _ -> { name = ""; descriptor = "placeholder"; interactions = []}

                let interactions = obj.interactions

                let interaction =
                    List.filter (fun i -> (
                        if List.contains input (List.map (fun (i: Interaction) -> i.name.ToLower()) interactions) then true else false
                        )
                    ) interactions |> List.head

                let effect: Effect = interaction.effect

                if effect = "game win" then 
                    printfn "You won! Thanks for playing."
                    exit 0

            | _ ->
                printfn "I'm sorry, that is invalid input. If you need help with valid commands, type 'help'. Please try again." 
                playGame gs 

        else
            printfn "You have lost!"
            exit 0

(*
    TODO Fix -1 dmg and -1 healing
    Fix diagnostic outside of combat
*)