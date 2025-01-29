
open System
open System.IO
open System.Collections.Generic

// https://github.com/dwyl/english-words/blob/master/words_alpha.txt

type MatchType =
|Letter
|LetterOrEmpty

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





type Word_state2 = { word: string; letter_position: int; candidate_Coordinates: seq<Coordinate> option }
type Word_state3 = { word: string; letter_position: int; can_add_word_here: Coordinate option}
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

seed_the_dictionaries source_words_2

//let printDictionary (dict: Dictionary<'Key, 'Value>) =
//    for KeyValue(key, value) in dict do
//        printfn "Key: %A, Value: %A" key value

//printDictionary letters

//let source_words = [ "hello"; "world"; "goodbye"; "cruel"; "worldx" ]

//let seed_the_dictionaries =

//    letters.Add('a', [{X=1;Y=2}])
//    letters.Add('b', [{X=2;Y=2}])   
//    letters.Add('c', [{X=3;Y=2}])   
//    letters.Add('d', [{X=4;Y=2}])   
//    letters.Add('e', [{X=5;Y=2}])   
//    letters.Add('f', [{X=6;Y=2}])
//    letters.Add('g', [{X=7;Y=2}])
//    letters.Add('h', [{X=8;Y=2}])
//    letters.Add('i', [{X=9;Y=2}])
//    letters.Add('j', [{X=10;Y=2}])
//    letters.Add('k', [{X=11;Y=2}])
//    letters.Add('l', [{X=12;Y=2}])
//    letters.Add('m', [{X=13;Y=2}])
//    letters.Add('n', [{X=14;Y=2}])
//    letters.Add('o', [{X=15;Y=2}])
//    letters.Add('p', [{X=16;Y=2}])
//    letters.Add('q', [{X=17;Y=2}])
//    letters.Add('r', [{X=18;Y=2}])
//    letters.Add('s', [{X=19;Y=2}])
//    letters.Add('t', [{X=20;Y=2}])
//    letters.Add('u', [{X=21;Y=2}])
//    letters.Add('v', [{X=22;Y=2}])
//    letters.Add('w', [{X=23;Y=2}])
//    letters.Add('x', [{X=24;Y=2}])
//    letters.Add('y', [{X=25;Y=2}])
//    letters.Add('z', [{X=26;Y=2}])


let returns_matching_letters_on_the_grid (source_words:string list) : seq<Word_state2> =

    seq {

        for word in source_words do
            let wordAsArray = word.ToCharArray()
            for i = 0 to wordAsArray.Length do
                    let found, res1 = letters.TryGetValue word.[i]
                    match found with
                    | true -> let res2 = Seq.ofList res1
                              yield { Word_state2.word=word; letter_position=i; candidate_Coordinates=Some res2 }
                    | _ ->    yield { Word_state2.word=word; letter_position=i; candidate_Coordinates=None }
        }

let random = Random()

let isCellEmpty (coordinate:Coordinate) =

    let found, res = coordinatesDict.TryGetValue coordinate

    found

let isCellAvailiable (c:char) (coordinate:Coordinate) (matchType:MatchType) = 

     let found, res = coordinatesDict.TryGetValue coordinate

     match (matchType, found) with
     | Letter , true        when res.Letter = c  -> true
     | Letter , true        when res.Letter <> c -> false
     | Letter , false                            -> false
     | LetterOrEmpty , true when res.Letter = c  -> true
     | LetterOrEmpty , true when res.Letter <> c -> false
     | LetterOrEmpty , false                     -> true
     | _                                         -> false

let moveToCoordinate start cellCountToMoveAndDirection lineOfTheWord =

    match lineOfTheWord with
    | ACROSS -> { start with X = start.X + cellCountToMoveAndDirection}
    | DOWN   -> { start with Y = start.Y + cellCountToMoveAndDirection}

let areCellsAvailiable (word:string) (offsetOfIntersectingLetter:int) (lineOfTheWord:Direction) (gridCoordinate:Coordinate) =

    // Note using position not offset in the movement calculations

    let lengthOfTheWord = word.Length
    let positionOfIntersectingLetter = offsetOfIntersectingLetter + 1

    let lettersBeforeTheIntersectionLetter = positionOfIntersectingLetter - 1
    let lettersAfterTheIntersectionLetter = lengthOfTheWord - positionOfIntersectingLetter

    let coordinateAdjacentToStartLetter         = [moveToCoordinate gridCoordinate (lettersBeforeTheIntersectionLetter + 1) lineOfTheWord]
    let coordinateAdjacentToEndLetter           = [moveToCoordinate gridCoordinate (lettersAfterTheIntersectionLetter + 1) lineOfTheWord]
    let coordinatesStartToIntersectingLetter    = [moveToCoordinate gridCoordinate (lettersBeforeTheIntersectionLetter) lineOfTheWord]@[gridCoordinate]

    let coordinatesAfterIntersectingToEndLetter = [gridCoordinate]@[moveToCoordinate gridCoordinate (lettersAfterTheIntersectionLetter) lineOfTheWord]@[gridCoordinate]
    let coordinatesAdjacentToEndLetter          = [moveToCoordinate gridCoordinate (lettersAfterTheIntersectionLetter + 1) lineOfTheWord]


    isCellAvailiable word.                                        (c:char) (coordinate:Coordinate) (matchType:MatchType) = 






let can_add_word_here (c:char) (coordinates:seq<Coordinate>) = 

    let update_overall_status this_coordinate overall_status =

        match (this_coordinate, overall_status) with
        | (Valid(xy1), AtleastOneValid)    -> overall_status
        | (Valid(xy1), _)                  -> AtleastOneValid
        | (NotValid(xy1), _)               -> overall_status

    let first_pass =
        coordinates |> 
        Seq.scan (fun state xy -> match random.Next(100) with
                                  | rnd when rnd > 0   -> {this_coordinate=Valid(xy)      ; overall_status=update_overall_status (Valid(xy)) state.overall_status }
                                  | _                  -> {this_coordinate=NotValid(xy)   ; overall_status=update_overall_status (NotValid(xy)) state.overall_status }
                                                        ) {this_coordinate=NotValid({X=0;Y=0}); overall_status=NoneValid}
        |> Seq.skip 1 // initial state (NoneValid)
        |> Seq.cache

    let res = first_pass 
              |> Seq.last
              |> fun state -> state.overall_status
              
    seq { match res with
          | AtleastOneValid -> 
                for state in first_pass do
                    match state.this_coordinate with
                    | Valid xy  -> yield Some(xy)
                    | _         -> yield! Seq.empty
          | _ -> yield None }


let return_status_of_candidate_coordinates (coordinates:seq<Word_state2>) : seq<Word_state3>  =

    seq {
        for coordinate_info in coordinates do
            match coordinate_info.candidate_Coordinates with
            | None   -> yield { Word_state3.word=coordinate_info.word; letter_selected=coordinate_info.letter_selected; can_add_word_here=None }
            | Some c -> let here = can_add_word_here coordinate_info.letter_selected c 
                        for xy in here do // this will be many Valid(xy) or all None
                            //printfn ("yield %A %A %A") coordinate_info.word coordinate_info.letter_selected xy
                            yield { Word_state3.word=coordinate_info.word; letter_selected=coordinate_info.letter_selected; can_add_word_here=xy }
        }

let return_one_coordinate_for_one_word (coordinates:seq<Word_state3>) : seq<Word_state3>  =

    coordinates |>
    Seq.distinctBy (fun state -> state.word)

let do_dict_updates word xy :unit = 

    //printfn ("word %A location %A") word xy

    ()

let Update_dictionaries_output_failed_words (valid_coordinate:seq<Word_state3>) =

    seq {
        for c in valid_coordinate do
            //printfn ("%A %A %A") c.word c.letter_selected c.can_add_word_here
            match c.can_add_word_here with 
            | Some(xy)    -> do_dict_updates c.word xy
            | _           -> yield c.word
    }

let rec update_the_dictionaries (source_words:string list) (length_of_previous_failed_list:int) =

    let failed_list =
        source_words 
        |> returns_seq_of_words_and_letters 
        |> returns_seq_of_coordinates_for_the_letters 
        |> return_status_of_candidate_coordinates 
        |> return_one_coordinate_for_one_word
        |> Update_dictionaries_output_failed_words
        |> Seq.toList

    printfn ("failed_list %A") failed_list.Length
    //printfn ("failed_list %A") failed_list
    Console.ReadLine() |> ignore

    match failed_list.Length with
    | l when l = length_of_previous_failed_list && l <> 0 -> failed_list // these words cannot be added.
    | l when l = 0 -> []                                                 // all words have been added.
    | _ -> update_the_dictionaries failed_list failed_list.Length        // retry the failed to add words

update_the_dictionaries source_words_2 0
|> ignore




