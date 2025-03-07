module Module_Common

open System.Collections.Generic

type Direction = ACROSS | DOWN

type MatchType =
| Letter
| LetterOrEmpty
| Empty

type CellContent =
| MatchingLetter
| NoMatchingLetter
| Empty

type CellStatus = {cellContent:CellContent ; availableDirection:Direction option}

type WordSplit = {offsetOfIntersectingLetter:int; positionOfIntersectingLetter:int; NumberOflettersBeforeTheIntersectionLetter:int; NumberOflettersAfterTheIntersectionLetter:int }

type Placed = { word:string; placed:bool}
type Coordinate = { X: int; Y: int }

type Coordinate_status =
| Valid of Coordinate
| NotValid of Coordinate

type Overall_coordinates_status =
| AtleastOneValid
| NoneValid

type Letter_status = Placed | Indirect
type DirectionOfMovement = ToStart | ToEnd

type Letter_info = { Letter: char; Down: Option<Letter_status>; Across: Option<Letter_status> }
type Word_start = { Word: string; Coordinate: Coordinate; Direction: Direction }

type For_dictionary_update = {word:string; intersection_coordinate:Coordinate ; coordinates_of_the_word:(Coordinate*char) seq ; new_word_direction:Direction}
let letters = Dictionary<char , Coordinate list>()


//		there will be a Coordinate 					Dictionary of Key (x,y)    	and Value ( {letter, Down=Some(Placed\Indirect) or None, Up=Some(Placed\Indirect) or None } )
let coordinatesDict = Dictionary<Coordinate , Letter_info>()

// all available xy for current word
let availableXYforWord = Dictionary<int,For_dictionary_update>()

// stats on letter dictionary lookups
type stats_on_letter_dictionary = {running_total_of_letter_dict_indexes:int; count_of_letter_dict_indexes:int}
let stats_on_letter_dictionary  = Dictionary<char,stats_on_letter_dictionary>()



//		there will be a Grid Placed Word			Dictionary of Key (word)   	and Value ({starting position (x,y), direction}).		
//let grid_placed_words = dict ([] : (string * Word_start) list)

//		there will be a Grid Indirect Word 			Dictionary of Key (word) 	and Value (list of records of {starting position (x,y), direction}).
//let grid_indirect_words = dict ([] : (string * Word_start) list)

//		there will be a Grid Indirect Invalid Word 	Dictionary of Key (word) 	and Value (list of records of {starting position (x,y), direction}).
//let grid_indirect_invalid_words = dict ([] : (string * Word_start) list)

type Position_on_the_grid = {can_add_word_here: Coordinate; for_dictionary_update: For_dictionary_update; letter_dict_index:int} 


type Word_state3_data   = { word: string; word_count:int; letter_position: int; position_on_the_grid: Position_on_the_grid option}
type Word_state3_marker = { end_of_records_marker_for_a_word: string; word_count:int}
type Word_state3 =
| DATA3   of Word_state3_data
| MARKER3 of Word_state3_marker


type Word_state3b_data   = { word: string; word_count:int; letter_position: int; letter_dict_index:int; can_add_word_here: Coordinate ; for_dictionary_update: For_dictionary_update}
type Word_state3b_marker = { end_of_records_marker_for_a_word: string; word_count:int}
type Word_state3b =
| DATA3b   of Word_state3b_data
| MARKER3b of Word_state3b_marker

type Word_state4_Intermediate = { word:string;  word_count:int; running_total_of_letter_dict_indexes:int; count_of_letter_dict_indexes:int; availableXYcounter:int }
type Word_state4_Final        = { word:string;  word_count:int; running_total_of_letter_dict_indexes:int; count_of_letter_dict_indexes:int; for_dictionary_update: For_dictionary_update option}

type Word_state4 =
| Intermediate of Word_state4_Intermediate
| Final        of Word_state4_Final

let (|NEW_WORD_BLOCK|IN_WORD_BLOCK|MARKER_AFTER_EMPTY_WORD_BLOCK|MARKER_AFTER_WORD_BLOCK|)( xy:Word_state3b, state:Word_state4) =
 
    match xy,state with
    | DATA3b a   , Final f        -> NEW_WORD_BLOCK (a , f)
    | DATA3b a   , Intermediate i -> IN_WORD_BLOCK (a , i)
    | MARKER3b a , Final f        -> MARKER_AFTER_EMPTY_WORD_BLOCK (a , f)
    | MARKER3b a , Intermediate i -> MARKER_AFTER_WORD_BLOCK (a , i)

type failed_list = seq<string>


let xy_letter_selection_limit = 5
let xy_word_selection_limit = xy_letter_selection_limit * 10
let housekeeping_required = 100








