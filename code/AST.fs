module AST

type Name = string
type Effect = string
type Type = string
type Descriptor = string
type Stats = { hp: int; 
               mp: int; 
               atk: int; 
               def: int; 
               matk: int; 
               mdef: int; 
               spd: int; }

type Ability = { name: Name;
                 effect: Effect; }
type Abilities = Ability list

type Connection = string * string
type Connections = Connection list

type Sentence =
| Character of Character 
| Object of Object
and Character = { typep: Type;
                  name: Name; 
                  stats: Stats; 
                  abilities: Abilities; }
and Object = { name: Name;
               descriptor: Descriptor;
               interactions: Effect list; }

type Room = { name: Name;
             descriptor: Descriptor;
             objects: Sentence list;
             connections: Connections; } 

type Paragraph = Room list