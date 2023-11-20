module AST

type Name = string
type Effect = string
type Type = string
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

type Interaction = { name: Name;
                     effect: Effect; }
type Interactions = Interaction list

type Connection = string
type Connections = Connection list

type Sentence =
| Character of Character
| Room of Room
| Object of Object
and Character = { typep: Type;
                  name: Name; 
                  stats: Stats; 
                  abilities: Abilities; }
and Room = { name: Name;
             objects: Object list;
             connections: Connections; }
and Object = { name: Name;
               interactables: Interactions; }

type Paragraph = Sentence list