module Parser

open Combinator
open AST

let paragraph, paragraphImpl = recparser()
let psentence, psentenceImpl = recparser()

let pad =
    fun p ->
        pbetween
            (pws0)
            (p)
            (pws0)

let pstrhelper = pletter <|> pchar ' '

let punknownstr = 
    pbetween
        (pchar '\"')
        (pmany1 (psat (fun c -> c <> '\"')))    
        (pchar '\"') 
        |>> stringify // pseq (pmany0 pupper) (pmany1 pletter) (fun (us, ls) -> stringify (us @ ls))
    
let pnumber = pmany1 pdigit |>> (fun ds -> stringify ds) |>> int

let ptype = 
    pleft
                (punknownstr <!> "Type String") 
                (pad (pstr "character"))
    |>> (fun t -> Type t) <!> "type"

let pname =
    pright
        (pad (pstr "named"))
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
        (pad (pstr "with stats:"))
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
        (pad (pstr "has effect"))
            (punknownstr <!> "Effect String")
    |>> (fun e -> Effect e) <!> "effect"
    
let pability: Parser<Ability> =
    pbetween
            (pad (pstr "ability"))
            (pseq
                (pname <!> "Ability String")
                (peffect)
                (fun a ->  {name = fst a; effect = snd a})  <!> "ability"
            )
            ((pstr ",") <|> (pstr ""))

let pabilities: Parser<Abilities> =
    pright
        (pad (pstr "and abilities:"))
        (pmany0 pability)
    |>> (fun ps -> ps) <!> "abilities"

// Character of Type * Name * Stats * Abilities
let character = 
    pright
        (pws0)
        (pbind ptype (fun t ->
            pbind pname (fun n ->
                    (pbind pstats (fun s ->
                        pbind pabilities (fun a ->
                            presult { typep = t; name = n; stats = s; abilities = a }
                            )
                        )
                    )
                )
            )  
        )
    |>> (fun c -> Character c) <!> "character"

let pdescriptor = 
    pright
        (pad (pstr "with description:"))
        (punknownstr)
    |>> (fun d -> Descriptor d) <!> "descriptor"

let pobjects: Parser<Sentence list> =
    pright
        (pad (pstr "with objects:"))
        (pmany0 psentence)  
    |>> (fun os -> os) <!> "objects"

let pconnection =
    pseq
        (pleft 
            (punknownstr)
            (pad (pstr "is"))
        )
        (pleft 
            (punknownstr)
        (pstr "," <|> pstr ".")
        )
        (fun c -> Connection c) <!> "connection"

let pconnections: Parser<Connections> =
    pright
        (pad (pstr "and connections:"))
        (pmany0 pconnection)
    |>> (fun cs -> cs) <!> "connections"

// There is a <room> <name>: with objects: <objects> and connections: <connections>
let room: Parser<Room> =
    pbetween
        (pws0)
        (pright 
            (pad (pstr "There is a room"))
            (pbind pname (fun n ->
                pbind pdescriptor (fun d->
                    pbind pobjects (fun os ->
                        pbind pconnections (fun c ->
                            presult { name = n; descriptor = d; objects = os; connections = c} 
                            )
                        )
                    )
                )
            )
        )
        (pstr "")
    |>> (fun r -> r) <!> "room" // WAS Room r before changing ast


psentenceImpl := character //<|> object 

paragraphImpl := pmany1 room |>> (fun (ss) -> ss)
let grammar = pleft paragraph peof

let parse =
    fun input ->
        let i = prepare input
        match grammar i with
        | Success(ast, _) -> Some ast
        | Failure(_, _) -> None