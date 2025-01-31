
open System
open System.IO
open System.Collections.Generic

// https://github.com/dwyl/english-words/blob/master/words_alpha.txt

type MatchType =
|Letter
|LetterOrEmpty
|Empty

type Placed = { word:string; placed:bool}
type Coordinate = { X: int; Y: int }

type Coordinate_status =
| Valid of Coordinate
| NotValid of Coordinate

type Overall_coordinates_status =
| AtleastOneValid
| NoneValid

type Letter_status = Placed | Indirect
type Direction = ACROSS | DOWN
type DirectionOfMovement = ToStart | ToEnd

type Letter_info = { Letter: char; Down: Option<Letter_status>; Across: Option<Letter_status> }
type Word_start = { Word: string; Coordinate: Coordinate; Direction: Direction }

let source = Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt")

let source_words   = [ "hello"; "world"; "goodbye"; "cruel"; "place" ]

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


type For_dictionary_update = {intersection_coordinate:Coordinate ; coordinates_of_the_word:(Coordinate*char) list ; new_word_direction:Direction}
type State = { this_coordinate: Coordinate_status; overall_status: Overall_coordinates_status ; for_dictionary_update: For_dictionary_update option }

type Word_state2 = { word: string; letter_position: int; candidate_Coordinates: seq<Coordinate> option }
type Word_state3 = { word: string; letter_position: int; can_add_word_here: Coordinate option; for_dictionary_update: For_dictionary_update option}
type failed_list = seq<string>

//let update_the_letters_the_dictionary (source_words_2: string list) =

//    let rec read_the_characters (x:int) (y:int) (position:int) (first_word:string) : unit =

//        match position < first_word.Length with
//        | true -> try
//                     let current_value = letters.Item(first_word.[position])
//                     let new_value = current_value@[{X=x;Y=y}]
//                     letters.Item(first_word.[position]) <- new_value
//                  with
//                  | :? System.Collections.Generic.KeyNotFoundException as ex -> 
//                       letters.Add(first_word[position], [{X=x;Y=y}])

//                  read_the_characters (x+1) y (position+1) first_word

//        | false -> ()

//    read_the_characters 0 0 0 source_words_2.Head

//update_the_letters_the_dictionary source_words
//update_the_letters_the_dictionary source_words_2

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
            for i = 0 to wordAsArray.Length - 1 do
                    let found, res1 = letters.TryGetValue word.[i]
                    match found with
                    | true -> let res2 = Seq.ofList res1
                              yield { Word_state2.word=word; letter_position=i; candidate_Coordinates=Some res2 }
                    | _ ->    yield { Word_state2.word=word; letter_position=i; candidate_Coordinates=None }
        }

let random = Random()

let isCellEmpty (coordinate:Coordinate) =

    let found, res = coordinatesDict.TryGetValue coordinate

    not found

let isCellAvailiable (coordinate:Coordinate) (c:char) (matchType:MatchType) = 

     let found, res = coordinatesDict.TryGetValue coordinate

     match (matchType , found) with
     | Letter         , true  when res.Letter = c  -> true
     | Letter         , true  when res.Letter <> c -> false
     | Letter         , false                      -> false
     | LetterOrEmpty  , true  when res.Letter = c  -> true
     | LetterOrEmpty  , true  when res.Letter <> c -> false
     | LetterOrEmpty  , false                      -> true
     | Empty          , false                      -> true
     | _                                           -> false

let directionForWordToBePlaced (coordinate:Coordinate) =

     let found, res = coordinatesDict.TryGetValue coordinate

     match found with
     | true ->  match (res.Down , res.Across) with
                | Some x , None   -> Some(ACROSS)  // select right-angles direction to existing word
                | None   , Some x -> Some(DOWN)
                | _               -> None
     | false -> None // should not get here. This subroutine only called when a dictionary key found.

let moveToCoordinate start cellCountToMove lineOfTheWord directionOfMovement =

    match cellCountToMove with
    | 0 -> []
    | _ ->  match (lineOfTheWord , directionOfMovement) with
            | ACROSS , ToStart -> [ for i in 1 .. cellCountToMove -> { start with X = start.X - i}] |> List.rev
            | ACROSS , ToEnd   -> [ for i in 1 .. cellCountToMove -> { start with X = start.X + i}]
            | DOWN   , ToStart -> [ for i in 1 .. cellCountToMove -> { start with Y = start.Y + i}] |> List.rev
            | DOWN   , ToEnd   -> [ for i in 1 .. cellCountToMove -> { start with Y = start.Y - i}] 

let checkAvailabilityOfRemainingCells (word:string) (offsetOfIntersectingLetter:int) (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate) =

    // Note using position not offset in the movement calculations
    let positionOfIntersectingLetter = offsetOfIntersectingLetter + 1

    let NumberOflettersBeforeTheIntersectionLetter = positionOfIntersectingLetter - 1
    let NumberOflettersAfterTheIntersectionLetter = word.Length - positionOfIntersectingLetter

    let coordinateAdjacentToStartLetter               = (moveToCoordinate gridCoordinate (NumberOflettersBeforeTheIntersectionLetter + 1) lineOfTheWordToBeAdded ToStart ).Head
    let coordinateAdjacentToEndLetter                 = (moveToCoordinate gridCoordinate (NumberOflettersAfterTheIntersectionLetter  + 1) lineOfTheWordToBeAdded ToEnd   ).Head


    let coordinatesStartUpToIntersectingLetter        = moveToCoordinate gridCoordinate NumberOflettersBeforeTheIntersectionLetter lineOfTheWordToBeAdded ToStart
    let coordinatesStartUpToIntersectingLetterAndChar = 
        match coordinatesStartUpToIntersectingLetter with
        | [] -> []
        | _ ->  [for i in 0 .. coordinatesStartUpToIntersectingLetter.Length - 1 -> (coordinatesStartUpToIntersectingLetter.[i] , word.[i]) ]


    let coordinatesAfterIntersectingToEndLetter        = moveToCoordinate gridCoordinate (NumberOflettersAfterTheIntersectionLetter) lineOfTheWordToBeAdded ToEnd
    let coordinatesAfterIntersectingToEndLetterAndChar =
        match coordinatesAfterIntersectingToEndLetter with
        | [] -> []
        | _ ->  [for i in 0  .. coordinatesAfterIntersectingToEndLetter.Length - 1 -> (coordinatesAfterIntersectingToEndLetter.[i] , word.[offsetOfIntersectingLetter + 1 + i]) ]

    if isCellAvailiable gridCoordinate word.[offsetOfIntersectingLetter] Letter &&
       isCellEmpty coordinateAdjacentToStartLetter && 
       isCellEmpty coordinateAdjacentToEndLetter && 
       coordinatesStartUpToIntersectingLetterAndChar  |> List.forall ( fun (xy,c) -> isCellAvailiable xy c LetterOrEmpty) &&
       coordinatesAfterIntersectingToEndLetterAndChar |> List.forall ( fun (xy,c) -> isCellAvailiable xy c LetterOrEmpty) then

       Some( {intersection_coordinate=gridCoordinate;
              coordinates_of_the_word=coordinatesStartUpToIntersectingLetterAndChar@[(gridCoordinate,word.[offsetOfIntersectingLetter])]@coordinatesAfterIntersectingToEndLetterAndChar;
              new_word_direction=lineOfTheWordToBeAdded} )

    else

       None
  
let areCellsAvailiable (word:string) (offsetOfIntersectingLetter:int) (gridCoordinate:Coordinate) =

    let availiableDirection = directionForWordToBePlaced gridCoordinate 

    match availiableDirection with
    | Some x -> match isCellAvailiable gridCoordinate word.[offsetOfIntersectingLetter] Letter with     
                | true -> checkAvailabilityOfRemainingCells word offsetOfIntersectingLetter x gridCoordinate
                | false -> None
    | None   -> None

let can_add_word_here (word:string) (offsetOfIntersectingLetter:int) (coordinates:seq<Coordinate>) = 

    let update_overall_status this_coordinate overall_status =

        match (this_coordinate, overall_status) with
        | (Valid(xy1), AtleastOneValid)    -> overall_status
        | (Valid(xy1), _)                  -> AtleastOneValid
        | (NotValid(xy1), _)               -> overall_status

    let first_pass =
        coordinates |> 
        Seq.scan (fun state xy -> match areCellsAvailiable word offsetOfIntersectingLetter xy with
                                  | Some x -> {this_coordinate=Valid(xy)      ; overall_status=update_overall_status (Valid(xy))    state.overall_status ; for_dictionary_update=Some x }
                                  | None   -> {this_coordinate=NotValid(xy)   ; overall_status=update_overall_status (NotValid(xy)) state.overall_status ; for_dictionary_update=None }
                                                        ) {this_coordinate=NotValid({X=0;Y=0}); overall_status=NoneValid; for_dictionary_update=None}
        |> Seq.skip 1 // initial state (NoneValid)
        |> Seq.cache

    let res = first_pass 
              |> Seq.last
              
    seq { match res.overall_status with
          | AtleastOneValid -> 
                for state in first_pass do
                    match state.this_coordinate with
                    | Valid xy  -> yield Some(res)
                    | _         -> yield! Seq.empty
          | _ -> yield None }


let return_status_of_candidate_coordinates (coordinates:seq<Word_state2>) : seq<Word_state3>  =

    let return_value x = 

        match x with
        | Valid xy -> xy
        | NotValid xy -> xy

    seq {
        for coordinate_info in coordinates do
            match coordinate_info.candidate_Coordinates with
            | None   -> yield { Word_state3.word=coordinate_info.word; letter_position=coordinate_info.letter_position; can_add_word_here=None; for_dictionary_update=None }
            | Some c -> let here = can_add_word_here coordinate_info.word coordinate_info.letter_position c 
                        for location_info in here do // this will be many Valid(xy) or all None
                            //printfn ("yield %A %A %A") coordinate_info.word coordinate_info.letter_selected xy
                            match location_info with
                            | Some l -> let xy = return_value l.this_coordinate
                                        yield { Word_state3.word=coordinate_info.word; letter_position=coordinate_info.letter_position; can_add_word_here=Some(xy); for_dictionary_update=l.for_dictionary_update }
                            | None   -> yield { Word_state3.word=coordinate_info.word; letter_position=coordinate_info.letter_position; can_add_word_here=None; for_dictionary_update=None }
        }

let return_one_coordinate_for_one_word (coordinates:seq<Word_state3>) : seq<Word_state3>  =

// could later make this a random selection

    coordinates |>
    Seq.distinctBy (fun state -> state.word)

let do_dict_updates (for_dictionary_update:For_dictionary_update) =

    for xy , letter in for_dictionary_update.coordinates_of_the_word do

           let found, res = coordinatesDict.TryGetValue xy

           match found with
           | true -> match for_dictionary_update.new_word_direction with
                     | ACROSS -> coordinatesDict.Item(xy) <- {res with Across=Some(Placed)} 
                     | DOWN   -> coordinatesDict.Item(xy) <- {res with Down=Some(Placed)} 
           | false -> 
                     match for_dictionary_update.new_word_direction with
                     | ACROSS -> coordinatesDict.Add(xy,{Letter_info.Letter=letter; Down=None;         Across=Some(Placed)} )
                     | DOWN   -> coordinatesDict.Add(xy,{Letter_info.Letter=letter; Down=Some(Placed); Across=None} )

                     let found, res2 = letters.TryGetValue letter

                     match found with
                     | true  -> letters.Item(letter) <- res2@[xy] // xy is a new location so can append. If xy was the intersect cell then that xy will all ready have been added.
                     | false -> letters.Add(letter,[xy])

    ()

let Update_dictionaries_output_failed_words (valid_coordinate:seq<Word_state3>) =

    seq {
        for c in valid_coordinate do
            //printfn ("%A %A %A") c.word c.letter_selected c.can_add_word_here
            match c.can_add_word_here with 
            | Some(xy)    -> match c.for_dictionary_update with
                             | Some(coordinate_info) -> do_dict_updates coordinate_info
                             | None -> yield! Seq.empty
            | _           -> yield c.word
    }

let seed_the_first_word (word:string) :unit =
    
    let starting_coordinate = {X=1;Y=2}   
    let coordinate_list_for_the_word = [starting_coordinate]@(moveToCoordinate starting_coordinate (word.Length - 1) ACROSS ToEnd)
    let coordinate_list_for_the_wordAndChar = [for i in 0 .. coordinate_list_for_the_word.Length - 1 -> (coordinate_list_for_the_word.[i] , word.[i])]

    do_dict_updates {intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=ACROSS}

let rec update_the_dictionaries (source_words:string list) (length_of_previous_failed_list:int) =

    let failed_list =
        source_words 
        |> returns_matching_letters_on_the_grid 
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

seed_the_first_word source_words.Head
update_the_dictionaries source_words.Tail 0 |> ignore




