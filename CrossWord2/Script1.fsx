
open System
open System.IO
open System.Collections.Generic

// https://github.com/dwyl/english-words/blob/master/words_alpha.txt


type Placed = { word:string; placed:bool}
type Coordinate = { X: int; Y: int }

type Coordinate_status =
| Valid of Coordinate
| NotValid of Coordinate

type Overall_coordinates_status =
| AtleastOneValid
| NoneValid

type State = { this_coordinate: Coordinate_status; overall_status: Overall_coordinates_status }

type Letter_status = Placed | Indirect
type Direction = ACROSS | DOWN

type Letter_info = { Letter: char; Down: Option<Letter_status>; Up: Option<Letter_status> }
type Word_start = { Word: string; Coordinate: Coordinate; Direction: Direction }

let source = Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt")

let source_words_2 =   System.IO.File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt"))
                       |> Array.filter (fun w -> w.Length > 1)
                       |> Array.toList


source_words_2.Length - 1 |> printfn "%d"

//		there will be a Letters						Dictionary of Key (letter) 	and Value (list (x,y))
// https://stackoverflow.com/questions/54074653/how-to-declare-an-empty-map-dictionary

// F# Core Dict (read-only)
//let letters   = dict Seq.empty<char * CoordinateList>
//let letters0  = dict ['a',[{X=1;Y=2}]]
//let letters1  = dict ([] :(char * CoordinateList) list)

let letters = Dictionary<char , Coordinate list>()

//		there will be a Coordinate 					Dictionary of Key (x,y)    	and Value ( {letter, Down=Some(Placed\Indirect) or None, Up=Some(Placed\Indirect) or None } )
let coordinatesDict = Dictionary<Coordinate , Letter_info>()

//		there will be a Grid Placed Word			Dictionary of Key (word)   	and Value ({starting position (x,y), direction}).		
let grid_placed_words = dict ([] : (string * Word_start) list)


//		there will be a Grid Indirect Word 			Dictionary of Key (word) 	and Value (list of records of {starting position (x,y), direction}).
let grid_indirect_words = dict ([] : (string * Word_start) list)

//		there will be a Grid Indirect Invalid Word 	Dictionary of Key (word) 	and Value (list of records of {starting position (x,y), direction}).
let grid_indirect_invalid_words = dict ([] : (string * Word_start) list)




type Word_state1 = { word: string; letter_selected: seq<char> }
type Word_state2 = { word: string; letter_selected: char; candidate_Coordinates: seq<Coordinate> option }
type Word_state3 = { word: string; letter_selected: char; can_add_word_here: Coordinate option}
type failed_list = seq<string>


let seed_the_dictionaries (source_words_2: string list) =

    let rec read_the_characters (x:int) (y:int) (position:int) (first_word:string) : unit =

        match position < first_word.Length with
        | true -> try
                     let current_value = letters.Item(first_word.[position])
                     let new_value = current_value@[{X=x;Y=y}]
                     letters.Item(first_word.[position]) <- new_value
                  with
                  | :? System.Collections.Generic.KeyNotFoundException as ex -> 
                       letters.Add(first_word[position], [{X=x;Y=y}])

                  read_the_characters (x+1) y (position+1) first_word

        | false -> ()

    read_the_characters 0 0 0 source_words_2.Head



let printDictionary (dict: Dictionary<'Key, 'Value>) =
    for KeyValue(key, value) in dict do
        printfn "Key: %A, Value: %A" key value

seed_the_dictionaries source_words_2

printDictionary letters


