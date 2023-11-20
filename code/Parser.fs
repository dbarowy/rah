module Parser

open Combinator
open AST

let paragraph, paragraphImpl = recparser()

let punknownstr = pmany1 pletter |>> stringify // pseq (pmany0 pupper) (pmany1 pletter) (fun (us, ls) -> stringify (us @ ls))
    
let pnumber = pmany1 pdigit |>> (fun ds -> stringify ds) |>> int

let ptype = 
    pbetween
        (pstr "There is a ")
                (punknownstr <!> "Type String") 
                (pstr " character ")
    |>> (fun t -> Type t) <!> "type"

let pname =
    pright
        (pstr "named ")
            (punknownstr <!> "Name String")
    |>> (fun n -> Name n) <!> "name"

let php =
    pright
        (pstr "hp = ")
        pnumber

let pmp =
    pright
        (pstr ", mp = ")
        pnumber

let patk =
    pright
        (pstr ", atk = ")
        pnumber

let pdef =
    pright
        (pstr ", def = ")
        pnumber

let pmatk =
    pright
        (pstr ", matk = ")
        pnumber

let pmdef =
    pright
        (pstr ", mdef = ")
        pnumber

let pspd =
    pright
        (pstr ", spd = ")
        pnumber   

let pstats =
    pright
        (pstr "stats: ")
        (pbind php (fun h ->
            pbind pmp (fun m ->
                pbind patk (fun a ->
                    pbind pdef (fun d ->
                        pbind pmatk (fun ma ->
                            pbind pmdef (fun md ->
                                pbind pspd (fun s ->
                                    presult { hp = h; mp = m; atk = a; def = d; matk = ma; mdef = md; spd = s }
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    |>> (fun s -> s)  <!> "stats"

let peffect =
    pright
        (pstr " has effect ")
            (punknownstr <!> "Effect String")
    |>> (fun e -> Effect e) <!> "effect"
    
let pability: Parser<Ability> =
    pbetween
            (pstr "ability ")
            (pseq
                (pname <!> "Ability String")
                (peffect)
                (fun a ->  {name = fst a; effect = snd a})  <!> "ability"
            )
            ((pstr ", ") <|> (pstr ""))

let pabilities: Parser<Abilities> =
    pright
        (pstr " and abilities: ")
        (pmany0 pability)
    |>> (fun (ps) -> ps) <!> "abilities"

// Character of Type * Name * Stats * Abilities
let character = 
    pbetween
        (pws0)
        (pbind ptype (fun t ->
            pbind pname (fun n ->
                    pright
                        (pstr " with ")
                        (pbind pstats (fun s ->
                            pbind pabilities (fun a ->
                                presult { typep = t; name = n; stats = s; abilities = a }
                            )
                        )
                    )
                )
            )  
        )
        (pchar '.')
    |>> (fun c -> Character c) <!> "character"

let psentence = character// <|> room <|> object 

paragraphImpl := pmany1 psentence |>> (fun ss -> ss)
let grammar = pleft paragraph peof

let parse(input: string) =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None