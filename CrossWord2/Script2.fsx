
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
//type State = { this_coordinate: Coordinate_status; overall_status: Overall_coordinates_status ; for_dictionary_update: For_dictionary_update option }
//type State = { this_coordinate: Coordinate_status; for_dictionary_update: For_dictionary_update option }


type Word_state2_data   = { word: string; letter_position: int; candidate_Coordinate: Coordinate option }
type Word_state2_marker = { end_of_records_marker_for_a_word: string}
type Word_state2 =
| DATA2   of Word_state2_data
| MARKER2 of Word_state2_marker


type Position_on_the_grid = {can_add_word_here: Coordinate; for_dictionary_update: For_dictionary_update} 


type Word_state3_data   = { word: string; letter_position: int; position_on_the_grid: Position_on_the_grid option}
type Word_state3_marker = { end_of_records_marker_for_a_word: string}
type Word_state3 =
| DATA3   of Word_state3_data
| MARKER3 of Word_state3_marker


type Word_state3b_data   = { word: string; letter_position: int; can_add_word_here: Coordinate ; for_dictionary_update: For_dictionary_update}
type Word_state3b_marker = { end_of_records_marker_for_a_word: string}
type Word_state3b =
| DATA3b   of Word_state3b_data
| MARKER3b of Word_state3b_marker

type AccStatus =
| Final
| Intermediate

type Word_state4  = { status: AccStatus; for_dictionary_update: For_dictionary_update list }
type Word_state5  = { for_dictionary_update: For_dictionary_update list }


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

//let return_a_word_records (word:string) : seq<Word_state2> =

//    seq {
//            printfn "return_a_word_records %A " word
//            let wordAsArray = word.ToCharArray()
//            for i = 0 to wordAsArray.Length - 1 do
//                    printfn "return_a_word_records %A " word.[i]
//                    let found, res1 = letters.TryGetValue word.[i]
//                    match found with
//                    | true -> for xy in res1 do
//                                yield { Word_state2.word=word; letter_position=i; candidate_Coordinates=Some xy }
//                    | _    -> yield { Word_state2.word=word; letter_position=i; candidate_Coordinates=None }
//        }

let returns_matching_letters_on_the_grid (source_words:list<string>) : seq<Word_state2> =

    seq {
        for word in source_words do
            printfn "returns_matching_letters_on_the_grid %A " word
            let wordAsArray = word.ToCharArray()
            for i = 0 to wordAsArray.Length - 1 do
                    printfn "return_a_word_records %A " word.[i]
                    let found, res1 = letters.TryGetValue word.[i]
                    match found with
                    | true -> for xy in res1 do
                                yield DATA2 { word=word; letter_position=i; candidate_Coordinate=Some xy }
                    | _    -> yield DATA2 { word=word; letter_position=i; candidate_Coordinate=None }
            yield MARKER2 { end_of_records_marker_for_a_word=word}
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

//let can_add_word_here (word:string) (offsetOfIntersectingLetter:int) (coordinates:seq<Coordinate>) = 

    //let first_pass =

    //    seq {  yield {this_coordinate=NotValid({X=0;Y=0}); for_dictionary_update=None}
        
    //           for xy in coordinates do
               
    //               match areCellsAvailiable word offsetOfIntersectingLetter xy with
    //               | Some x -> yield {this_coordinate=Valid(xy) ; for_dictionary_update=Some x }
    //               | None   -> yield! Seq.empty
        
    //    } 

    //let first_pass_cache = first_pass |> Seq.cache


    //seq { match first_pass_cache |> Seq.length with
    //      | 1 -> yield None
    //      | _ -> for state in (first_pass_cache |> Seq.skip 1) do // first entry is that default yield so the sequence always returns at least one record.
    //                 match state.this_coordinate with
    //                 | Valid xy  -> yield Some(state)
    //                 | _         -> yield! Seq.empty
    //    }




let return_status_of_candidate_coordinates (coordinates:seq<Word_state2>) : seq<Word_state3>  =

    let return_value x = 

        match x with
        | Valid xy -> xy
        | NotValid xy -> xy

  
    // output a streamof these { word ; position ; aCandidateGridXY } plus end-of-word marker records

    // with the marker record included Seq.Scan {..} can yield a new marker record indicating 
    // if None of the candidate xy for a word results in a valid place for the word to be added to the grid
    // or a marker record containing a list of all valid(xy) for a word.

    // a Seq.Filter can then be used to select just these modified marker records.

    // one of the valid xy can be selected (randomly) to update the dictionary
    // None marker records can be yielded for passing back in for a re-try.

    //seq {
    //    printfn "return_status_of_candidate_coordinates"
    //    for coordinate_info in coordinates do
    //        printfn "return_status_of_candidate_coordinates %A %A " coordinate_info.word coordinate_info.letter_position
    //        //printfn "%A %A" coordinate_info.word coordinate_info.letter_position
    //        match coordinate_info.candidate_Coordinates with
    //        | None   -> yield { Word_state3.word=coordinate_info.word; letter_position=coordinate_info.letter_position; can_add_word_here=None; for_dictionary_update=None }
    //        | Some c -> let here = can_add_word_here coordinate_info.word coordinate_info.letter_position c 
    //                    for location_info in here do // this will be many Valid(xy) or a single None
    //                        //printfn ("can add word here %A") location_info
    //                        match location_info with
    //                        | Some l -> let xy = return_value l.this_coordinate
    //                                    yield { Word_state3.word=coordinate_info.word; letter_position=coordinate_info.letter_position; can_add_word_here=Some(xy); for_dictionary_update=l.for_dictionary_update }
    //                        | None   -> yield { Word_state3.word=coordinate_info.word; letter_position=coordinate_info.letter_position; can_add_word_here=None; for_dictionary_update=None }
    //    }

    coordinates
    |> Seq.scan (fun state xy -> match xy with
                                 | DATA2 a -> match a.candidate_Coordinate with
                                              | None   -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=None }
                                              | Some c -> let here = areCellsAvailiable a.word a.letter_position c
                                                          match here with
                                                          | Some h -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=Some{can_add_word_here=c; for_dictionary_update=h} }
                                                          | None   -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=None }
                                 | MARKER2 b -> MARKER3 { end_of_records_marker_for_a_word=b.end_of_records_marker_for_a_word}
    
    ) (DATA3 { word="";letter_position=0; position_on_the_grid=None})
    |> Seq.skip 1 // omit the initial state


let collect_the_valid_coordinates (coordinates:seq<Word_state3>)  =
 
  seq { for coordinate in coordinates do
  
            match coordinate with
            | DATA3 a -> match a.position_on_the_grid with
                         | Some p -> yield (DATA3b { word=a.word; letter_position=a.letter_position; can_add_word_here=p.can_add_word_here ; for_dictionary_update=p.for_dictionary_update})
                         | None   -> yield! Seq.empty
            | MARKER3 b -> yield MARKER3b { end_of_records_marker_for_a_word=b.end_of_records_marker_for_a_word}
       }

  |> Seq.scan(fun (state:Word_state4) xy -> match xy with
                                            | DATA3b a   -> match state.status with
                                                            | Final        -> {Word_state4.status=Intermediate; for_dictionary_update=[a.for_dictionary_update]}
                                                            | Intermediate -> {state with status=Intermediate; for_dictionary_update=state.for_dictionary_update@[a.for_dictionary_update]}
                                            | MARKER3b b -> {state with status=Final}
    ) ({Word_state4.status=Final; for_dictionary_update=[]})
  
  |> Seq.skip 1 // omit the initial state
  |> Seq.filter(fun c -> match c.status with
                         | Final -> true
                         | _ -> false)
  |> Seq.map(fun c -> {Word_state5.for_dictionary_update = c.for_dictionary_update} )


  WORKING HERE

let return_one_coordinate_for_one_word (dictionary_data:seq<Word_state5>) =

 seq { for c in dictionary_data do
 
       let valid_XY_count = c.for_dictionary_update.Length
       let indx = random.Next(0, valid_XY_count)
       let selected_a_Coordinate = c.for_dictionary_update.[indx]
       yield selected_a_Coordinate
     }


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
    printfn "Update_dictionaries_output_failed_words"
    seq {
        for c in valid_coordinate do
            printfn ("Update_dictionaries_output_failed_words %A %A %A %A") c.word c.letter_position c.can_add_word_here c.for_dictionary_update
            printfn "   "
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

    printfn ("failed_list length ========================================= %A") failed_list.Length
    printfn ("failed_list %A") failed_list
    Console.ReadLine() |> ignore

    match failed_list.Length with
    | l when l = length_of_previous_failed_list && l <> 0 -> failed_list // these words cannot be added.
    | l when l = 0 -> []                                                 // all words have been added.
    | _ -> update_the_dictionaries failed_list failed_list.Length        // retry the failed to add words



// ======================================================

//let coordinatesDict = Dictionary<Coordinate , Letter_info>()
//coordinatesDict.Add({X=1;Y=2} , { Letter='c'; Down=Some(Placed); Across=Some(Placed)})
//isCellEmpty {X=1;Y=22}
//printfn "%A" coordinatesDict
//coordinatesDict.TryGetValue {X=1;Y=22}

//let coordinatesDict = Dictionary<Coordinate , Letter_info>()
//coordinatesDict.Add({X=1;Y=2} , { Letter='a'; Down=Some(Placed); Across=Some(Placed)})
//isCellAvailiable {X=1;Y=2} 'b' LetterOrEmpty 
//printfn "%A" coordinatesDict

//coordinatesDict.Add({X=1;Y=2} , { Letter='a'; Down=Some(Placed); Across=None})
//directionForWordToBePlaced {X=1;Y=22}
//printfn "%A" coordinatesDict

//moveToCoordinate {X=1;Y=2} 5 ACROSS ToEnd

let helperprintout x y =

     let found, res = coordinatesDict.TryGetValue {X=x;Y=y}
     //printfn "%A %A " found {X=x;Y=y}
     match found with
     | true -> printf "%A" res.Letter
     | false -> printf " "

let printBlock a b c d =

    for y in a .. -1 .. b do
        printfn ""
        for x in c .. d do
            helperprintout x y

//coordinatesDict.Add({X=1;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=2;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=3;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=4;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=5;Y=2}   , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=6;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=7;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=8;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=9;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=10;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=10;Y=6}  , { Letter='x'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=11;Y=2}  , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=12;Y=2}  , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=13;Y=2}  , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=14;Y=2}  , { Letter='l'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=15;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=15;Y=7}  , { Letter='x'; Down=None; Across=Some(Placed)})
    
//checkAvailabilityOfRemainingCells "world" 0 DOWN {X=1;Y=2} // true
//checkAvailabilityOfRemainingCells "world" 1 DOWN {X=2;Y=2} // true
//checkAvailabilityOfRemainingCells "world" 2 DOWN {X=3;Y=2} // true
//checkAvailabilityOfRemainingCells "world" 3 DOWN {X=4;Y=2} // true
//checkAvailabilityOfRemainingCells "world" 4 DOWN {X=5;Y=2} // true

//checkAvailabilityOfRemainingCells "world" 4 DOWN {X=10;Y=2} // false
//checkAvailabilityOfRemainingCells "world" 4 DOWN {X=15;Y=2} // false

// test 2 

//coordinatesDict.Add({X=1;Y=2}   , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=3}   , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=4}   , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=5}   , { Letter='l'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=6}   , { Letter='d'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=7}   , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=8}   , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=9}   , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=10}  , { Letter='l'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=11}  , { Letter='d'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=(-3);Y=11}  , { Letter='x'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=1;Y=12}  , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=13}  , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=14}  , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=15}  , { Letter='l'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=1;Y=16}  , { Letter='d'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=(-3);Y=16}  , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=(-4);Y=16}  , { Letter='x'; Down=Some(Placed); Across=None})
    
//checkAvailabilityOfRemainingCells "world" 0 ACROSS {X=1;Y=2} // true
//checkAvailabilityOfRemainingCells "world" 1 ACROSS {X=1;Y=3} // true
//checkAvailabilityOfRemainingCells "world" 2 ACROSS {X=1;Y=4} // true
//checkAvailabilityOfRemainingCells "world" 3 ACROSS {X=1;Y=5} // true
//checkAvailabilityOfRemainingCells "world" 4 ACROSS {X=1;Y=6} // true

//checkAvailabilityOfRemainingCells "world" 4 ACROSS {X=1;Y=11} // false
//checkAvailabilityOfRemainingCells "world" 4 ACROSS {X=1;Y=16} // false


//printBlock 12 -4 -3 20
//printfn "%A" coordinatesDict
//for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value




//coordinatesDict.Add({X=1;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=2;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=3;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=4;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=5;Y=2}   , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=6;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=7;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=8;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=9;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=10;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=10;Y=6}  , { Letter='x'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=11;Y=2}  , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=12;Y=2}  , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=13;Y=2}  , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=14;Y=2}  , { Letter='l'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=15;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=15;Y=7}  , { Letter='x'; Down=None; Across=Some(Placed)})

//areCellsAvailiable "world" 0 {X=1;Y=2} // true
//areCellsAvailiable "world" 1 {X=2;Y=2} // true

//areCellsAvailiable "world" 2 {X=3;Y=2} // true
//areCellsAvailiable "world" 3 {X=3;Y=2} // false

//areCellsAvailiable "world" 4 {X=10;Y=2} //false
//areCellsAvailiable "world" 4 {X=15;Y=2} // false

// test 2 

//coordinatesDict.Add({X=1;Y=2}   , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=3}   , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=4}   , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=5}   , { Letter='l'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=6}   , { Letter='d'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=7}   , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=8}   , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=9}   , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=10}  , { Letter='l'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=11}  , { Letter='d'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=(-3);Y=11}  , { Letter='x'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=1;Y=12}  , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=13}  , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=14}  , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=1;Y=15}  , { Letter='l'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=1;Y=16}  , { Letter='d'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=(-3);Y=16}  , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=(-4);Y=16}  , { Letter='x'; Down=Some(Placed); Across=None})
    
//areCellsAvailiable "world" 0 {X=1;Y=2} // true
//areCellsAvailiable "world" 1 {X=1;Y=3} // true
//areCellsAvailiable "world" 2 {X=1;Y=4} // true
//areCellsAvailiable "world" 3 {X=1;Y=5} // true

//areCellsAvailiable "world" 4 {X=1;Y=6} // true
//areCellsAvailiable "world" 3 {X=1;Y=6} // false

//areCellsAvailiable "world" 4 {X=1;Y=11} // false
//areCellsAvailiable "world" 4 {X=1;Y=16} // false

//directionForWordToBePlaced {X=2;Y=2}
//isCellAvailiable {X=2;Y=2} 'o' Letter

//for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value




//coordinatesDict.Add({X=1;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=2;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=3;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=4;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=5;Y=2}   , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=6;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=7;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=8;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=9;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=10;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=10;Y=6}  , { Letter='x'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=11;Y=2}  , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=12;Y=2}  , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=13;Y=2}  , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=14;Y=2}  , { Letter='l'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=15;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=15;Y=7}  , { Letter='x'; Down=None; Across=Some(Placed)})

//can_add_word_here "world" 0 (seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=10;Y=6} } )
//can_add_word_here "world" 4 (seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=10;Y=6};{X=5;Y=2};{X=15;Y=2} } )


//coordinatesDict.Add({X=1;Y=2}   , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=2;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=3;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=4;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=5;Y=2}   , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=6;Y=2}   , { Letter='w'; Down=Some(Placed); Across=Some(Placed)})
//coordinatesDict.Add({X=7;Y=2}   , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=8;Y=2}   , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=9;Y=2}   , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=10;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=11;Y=2}  , { Letter='w'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=12;Y=2}  , { Letter='o'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=13;Y=2}  , { Letter='r'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=14;Y=2}  , { Letter='l'; Down=None; Across=Some(Placed)})
//coordinatesDict.Add({X=15;Y=2}  , { Letter='d'; Down=None; Across=Some(Placed)})

//coordinatesDict.Add({X=15;Y=6} , { Letter='w'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=15;Y=5} , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=15;Y=4} , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=15;Y=3} , { Letter='l'; Down=Some(Placed); Across=None})
//// coordinatesDict.Add({X=15;Y=2} , { Letter='d'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=2;Y=3}    , { Letter='w'; Down=Some(Placed); Across=None})
//// coordinatesDict.Add({X=2;Y=2}    , { Letter='o'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=2;Y=1}    , { Letter='r'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=2;Y=0}    , { Letter='l'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=2;Y=(-1)} , { Letter='d'; Down=Some(Placed); Across=None})

//coordinatesDict.Add({X=6;Y=3}  , { Letter='x'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=6;Y=1}  , { Letter='x'; Down=Some(Placed); Across=None})
//coordinatesDict.Add({X=11;Y=0} , { Letter='x'; Down=None; Across=Some(Placed)})


//can_add_word_here "world" 0 (seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=15;Y=6};{X=2;Y=3} } )
//|> Seq.iter(fun c -> printfn "%A" c)


//can_add_word_here "world" 1 (seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=15;Y=6};{X=2;Y=3} } )
//|> Seq.iter(fun c -> printfn "%A" c)


//return_status_of_candidate_coordinates (seq { { word="world"; letter_position=0; candidate_Coordinates=Some(seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=15;Y=6};{X=2;Y=3} })};
//                                              { word="world"; letter_position=1; candidate_Coordinates=Some(seq { {X=2;Y=2};{X=7;Y=2};{X=12;Y=2};{X=15;Y=5}           })};
//                                              { word="world"; letter_position=2; candidate_Coordinates=None                                                           }
//                                            })
//|> Seq.iter(fun c -> printfn "%A" c)

//return_status_of_candidate_coordinates (seq { { word="world"; letter_position=0; candidate_Coordinates=Some(seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=15;Y=6};{X=2;Y=3} })};
//                                              { word="world"; letter_position=1; candidate_Coordinates=Some(seq { {X=2;Y=2};{X=7;Y=2};{X=12;Y=2};{X=15;Y=5}           })};
//                                              { word="world"; letter_position=2; candidate_Coordinates=None                                                           }
//                                            })
//|> return_one_coordinate_for_one_word
//|> Seq.iter(fun c -> printfn "%A" c)


//return_status_of_candidate_coordinates (seq { { word="world"; letter_position=0; candidate_Coordinates=Some(seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=15;Y=6};{X=2;Y=3} })};
//                                              { word="world"; letter_position=1; candidate_Coordinates=Some(seq { {X=2;Y=2};{X=7;Y=2};{X=12;Y=2};{X=15;Y=5}           })};
//                                              { word="world"; letter_position=2; candidate_Coordinates=None                                                           }
//                                            })
//|> return_one_coordinate_for_one_word
//|> Update_dictionaries_output_failed_words
//|> Seq.toList
//|> Seq.iter(fun c -> printfn "%A" c)


//return_status_of_candidate_coordinates (seq { { word="xxxxx"; letter_position=0; candidate_Coordinates=Some(seq { {X=1;Y=2};{X=6;Y=2};{X=11;Y=2};{X=15;Y=6};{X=2;Y=3} })};
//                                              { word="yyyyy"; letter_position=1; candidate_Coordinates=Some(seq { {X=2;Y=2};{X=7;Y=2};{X=12;Y=2};{X=15;Y=5}           })};
//                                              { word="qqqqq"; letter_position=2; candidate_Coordinates=None                                                           }
//                                            })
//|> return_one_coordinate_for_one_word
//|> Update_dictionaries_output_failed_words
//|> Seq.toList
//|> Seq.iter(fun c -> printfn "%A" c)


//return_status_of_candidate_coordinates (seq { { word="world"; letter_position=0; candidate_Coordinates=None };
//                                              { word="world"; letter_position=1; candidate_Coordinates=None };
//                                              { word="world"; letter_position=2; candidate_Coordinates=None }
//                                            })
//|> return_one_coordinate_for_one_word
//|> Seq.iter(fun c -> printfn "%A" c)




//seed_the_first_word "world"

//for kvp in letters         do printfn "Key: %A, Value: %A" kvp.Key kvp.Value
//for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value


seed_the_first_word source_words.Head
update_the_dictionaries ["cruel";"cruelA"] 0 |> ignore

//update_the_dictionaries source_words.Tail 0 |> ignore



for kvp in letters         do printfn "Key: %A, Value: %A" kvp.Key kvp.Value
for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value




