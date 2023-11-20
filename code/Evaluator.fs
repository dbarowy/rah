module Evaluator

open AST

module gameEngine =
    type GameState = { characters: Character list;
                       rooms: Room list;
                       objects: Object list;
                       turn: int;
                       alive: bool; }

    type Commands = 
    | Attack
    | Defend
    | Abilities
    | Flee

    let init = { characters = [];
                                  rooms = [];
                                  objects = [];
                                  turn = 0;
                                  alive = true; }

    let addCharacter char = 
        // printfn "Adding character..."
        { characters = char::init.characters;
          rooms = [];
          objects = [];
          turn = 0;
          alive = true; }

    let prettyprintCharacterInfo (char: Character) = 
        printf "There is a character named %s " char.name 
        printf "with stats %A " char.stats
        printf "and abilities %A " char.abilities 

    let gameStart mostRecentGameState = 
        printfn "Welcome to the game you've made! (TODO Allow user to create introduction)"
        printfn "Here is what is in the game so far: "
        printfn "%A" (prettyprintCharacterInfo mostRecentGameState.characters.Head)


(*


*)
let evalCharacter =
    fun c ->
        //printfn "Eval character..."
        gameEngine.addCharacter c 

let evalSentence =
    fun s ->
        match s with
        | Character {typep = t; name = n; stats = s; abilities = a} -> 
            let characterE = {typep = t; name = n; stats = s; abilities = a}
            //printfn "Eval sentence..."
            evalCharacter characterE
        | _ -> failwith "TODO"
        // | Room (n, i, c) -> 
        // | Object (n, i) ->  

let evalParagraph = 
    fun p  ->
        //printfn "Eval paragraph..."
        let mostRecentGameState = (List.map (fun s -> evalSentence s) p) |> List.last
        //printfn "Starting game..."
        gameEngine.gameStart mostRecentGameState