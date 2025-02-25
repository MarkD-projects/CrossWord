
open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Diagnostics

// https://github.com/dwyl/english-words/blob/master/words_alpha.txt

type MatchType =
| Letter
| LetterOrEmpty
| Empty

type CellStatus =
| MatchingLetter
| NoMatchingLetter
| Empty

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


type For_dictionary_update = {word:string; intersection_coordinate:Coordinate ; coordinates_of_the_word:(Coordinate*char) list ; new_word_direction:Direction}
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

type Word_state4  = { status: AccStatus; word:string; for_dictionary_update: For_dictionary_update list }
type Word_state5  = { word:string; for_dictionary_update: For_dictionary_update list }
type Word_state6  = { word:string; for_dictionary_update: For_dictionary_update option }

type failed_list = seq<string>

// DEBUG ============================================
let mutable word_to_print = ""
let mutable word_count_last_batch = 0
let mutable word_count_this_batch = 0
let printText state = 
    printfn "%-20s %-10i %-10i " word_to_print word_count_this_batch (word_count_this_batch - word_count_last_batch)
    word_count_last_batch <- word_count_this_batch
    word_count_this_batch <- 0
let timer = new Timer(printText, null, 0, 10000)
// ==================================================

let returns_matching_letters_on_the_grid (source_words:list<string>) : seq<Word_state2> =

    seq {
        for word in source_words do

            word_to_print <- word
            word_count_this_batch <- (word_count_this_batch + 1)

            let wordAsArray = word.ToCharArray()
            for i = 0 to wordAsArray.Length - 1 do
                    //printfn "return_a_word_records %A " word.[i]
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

     match (matchType  , found) with
     | Letter          , true  when res.Letter = c  -> true
     | Letter          , true  when res.Letter <> c -> false
     | Letter          , false                      -> false
     | LetterOrEmpty   , true  when res.Letter = c  -> true
     | LetterOrEmpty   , true  when res.Letter <> c -> false
     | LetterOrEmpty   , false                      -> true
     | MatchType.Empty , false                      -> true
     | _                                            -> false

let cellStatus (coordinate:Coordinate) (c:char) : CellStatus = 

     let found, res = coordinatesDict.TryGetValue coordinate

     match found with
     | true  when res.Letter = c  -> MatchingLetter
     | true  when res.Letter <> c -> NoMatchingLetter
     | _                          -> CellStatus.Empty

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

let returnAdjacentCellsXY (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate) =

    match lineOfTheWordToBeAdded with
    | ACROSS -> [{X=gridCoordinate.X;     Y=gridCoordinate.Y + 1} ; {X=gridCoordinate.X     ; Y=gridCoordinate.Y - 1}]
    | DOWN   -> [{X=gridCoordinate.X - 1; Y=gridCoordinate.Y}     ; {X=gridCoordinate.X + 1 ; Y=gridCoordinate.Y}    ]

//let coordinate_would_append_to_a_word_at_right_angles (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate)  =

//    List [ for xy in (returnAdjacentCellsXY lineOfTheWordToBeAdded gridCoordinate ) do
       
//           let found, res = coordinatesDict.TryGetValue xy

//           match found with
//           | true -> match lineOfTheWordToBeAdded with
//                     | ACROSS -> if res.Down.IsSome then yield xy
//                     | DOWN   -> if res.Across.IsSome then yield xy
//           | false -> yield! List.empty

//         ]

let no_adjacent_word_to_the_added_letter (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate)  =

    returnAdjacentCellsXY lineOfTheWordToBeAdded gridCoordinate
    |> 
    Seq.forall( fun xy -> let found, res = coordinatesDict.TryGetValue xy

                          match not found with
                          | true  -> true
                          | false -> false )

let checkAvailabilityOfRemainingCells (word:string) (offsetOfIntersectingLetter:int) (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate) =

    // Note using position not offset in the movement calculations
    let positionOfIntersectingLetter = offsetOfIntersectingLetter + 1

    let NumberOflettersBeforeTheIntersectionLetter = positionOfIntersectingLetter - 1
    let NumberOflettersAfterTheIntersectionLetter  = word.Length - positionOfIntersectingLetter

    let coordinateAdjacentToStartLetter() = (moveToCoordinate gridCoordinate (NumberOflettersBeforeTheIntersectionLetter + 1) lineOfTheWordToBeAdded ToStart ).Head
    let coordinateAdjacentToEndLetter()   = (moveToCoordinate gridCoordinate (NumberOflettersAfterTheIntersectionLetter  + 1) lineOfTheWordToBeAdded ToEnd   ).Head

    let coordinatesStartUpToIntersectingLetterAndChar() = 
        let coordinatesStartUpToIntersectingLetter = moveToCoordinate gridCoordinate NumberOflettersBeforeTheIntersectionLetter lineOfTheWordToBeAdded ToStart
        match coordinatesStartUpToIntersectingLetter with
        | [] -> []
        | _ ->  [for i in 0 .. coordinatesStartUpToIntersectingLetter.Length - 1 -> (coordinatesStartUpToIntersectingLetter.[i] , word.[i]) ]

    let coordinatesAfterIntersectingToEndLetterAndChar() =
        let coordinatesAfterIntersectingToEndLetter        = moveToCoordinate gridCoordinate (NumberOflettersAfterTheIntersectionLetter) lineOfTheWordToBeAdded ToEnd
        match coordinatesAfterIntersectingToEndLetter with
        | [] -> []
        | _ ->  [for i in 0  .. coordinatesAfterIntersectingToEndLetter.Length - 1 -> (coordinatesAfterIntersectingToEndLetter.[i] , word.[offsetOfIntersectingLetter + 1 + i]) ]

    let areCellsAvailable coor =
        coor
        |> Seq.forall ( fun (xy,c) -> match cellStatus xy c with                                  
                                      |MatchingLetter   -> true   
                                      |CellStatus.Empty -> no_adjacent_word_to_the_added_letter lineOfTheWordToBeAdded xy
                                      |_                -> false )

    // ======================================================================================================================

    // Do any of the letters to be added append to the beginning or end of an existing word?
    // 1] read letter list and return just thoses letter that will be added to empty cells
    // 2] return (x,y) of adjacent, right-angle, populated cells and the letter at that cell has the opposite Direction.

    // example, word D is not in a valid place. As it appends to words A and B. The intersection with C and E are okay.

    // Handled by coordinate_would_append_to_a_word_at_right_angles

    (*
          D
        CCCCC       << C is the chosen intersection letter to be used to place the new word
          D
    AAAAAADBBBBB    << the criteria above will return A and B
          D
       EEEEEEE      << intersection with existing letter E. So that letter is not returned.
          D

    *)

    // let gridCellCollisions = [for xy in gridCellsToBePopulated do yield! (coordinate_would_append_to_a_word_at_right_angles lineOfTheWordToBeAdded xy)] 

    // ======================================================================================================================

    // gridCellCollisions is to be replaced with a check that there are no adjacent letters along the side of the word.
    // the coding difference is that those letters can be flagged as ACROSS or DOWN and not just at right-angles.
    // note, the two coordinates at the start and end of the word that is to be added are checked for being empty elsewhere.

    // ======================================================================================================================

    // if isCellAvailiable gridCoordinate word.[offsetOfIntersectingLetter] Letter &&  // not required.

    if isCellEmpty (coordinateAdjacentToStartLetter()) then 

       if isCellEmpty (coordinateAdjacentToEndLetter()) then

          let coordinatesStartUpToIntersectingLetterAndChar = coordinatesStartUpToIntersectingLetterAndChar()

          if coordinatesStartUpToIntersectingLetterAndChar |> areCellsAvailable then

             let coordinatesAfterIntersectingToEndLetterAndChar = coordinatesAfterIntersectingToEndLetterAndChar()

             if coordinatesAfterIntersectingToEndLetterAndChar |> areCellsAvailable then

                Some( {word=word;
                       intersection_coordinate=gridCoordinate;
                       coordinates_of_the_word=coordinatesStartUpToIntersectingLetterAndChar@[(gridCoordinate,word.[offsetOfIntersectingLetter])]@coordinatesAfterIntersectingToEndLetterAndChar;
                       new_word_direction=lineOfTheWordToBeAdded} )

             else

                None

          else

             None

       else

          None
     
     else

        None 

let areCellsAvailiable (word:string) (offsetOfIntersectingLetter:int) (gridCoordinate:Coordinate) =

    let availiableDirection = directionForWordToBePlaced gridCoordinate 

    //match availiableDirection with
    //| Some x -> match isCellAvailiable gridCoordinate word.[offsetOfIntersectingLetter] Letter with     
    //            | true  -> checkAvailabilityOfRemainingCells word offsetOfIntersectingLetter x gridCoordinate
    //            | false -> failwithf "cell should be available %A %A %A " gridCoordinate word offsetOfIntersectingLetter
    //                       None
    //| None   -> None

    match availiableDirection with
    | Some x -> checkAvailabilityOfRemainingCells word offsetOfIntersectingLetter x gridCoordinate
    | None   -> None

let return_status_of_candidate_coordinates (coordinates:seq<Word_state2>) : seq<Word_state3>  =

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
                                                            | Final        -> {Word_state4.status=Intermediate; word=a.word; for_dictionary_update=[a.for_dictionary_update]}
                                                            | Intermediate -> {state with status=Intermediate; for_dictionary_update=state.for_dictionary_update@[a.for_dictionary_update]}
                                            | MARKER3b b -> match state.status with
                                                            | Final        -> {state with status=Final ; word=b.end_of_records_marker_for_a_word; for_dictionary_update=[]} // final followed by final means that flag has no preceeding valid coodinates
                                                            | Intermediate -> {state with status=Final} // the previous record was the last of the valid coordinates

    ) ({Word_state4.status=Final; word=""; for_dictionary_update=[]})
  
  |> Seq.skip 1 // omit the initial state
  |> Seq.filter(fun c -> match c.status with
                         | Final -> true
                         | _ -> false)
  |> Seq.map(fun c -> {Word_state5.word=c.word; for_dictionary_update = c.for_dictionary_update} )


let return_one_coordinate_for_one_word (dictionary_data:seq<Word_state5>) =

 seq { for c in dictionary_data do
 
       match c.for_dictionary_update with
       | [] -> yield {Word_state6.word =c.word; for_dictionary_update=None }
       | _  -> let valid_XY_count = c.for_dictionary_update.Length
               let indx = random.Next(0, valid_XY_count)
               let selected_a_Coordinate = c.for_dictionary_update.[indx]
               yield {Word_state6.word =c.word; for_dictionary_update=Some(selected_a_Coordinate) }
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

let Update_dictionaries_output_failed_words (dictionary_data:seq<Word_state6>) =
    // printfn "Update_dictionaries_output_failed_words"
    seq {
        for c in dictionary_data do
            match c.for_dictionary_update with 
            | Some coordinate_info -> do_dict_updates coordinate_info
                                      yield! Seq.empty
            | _                    -> yield c.word
    }

let seed_the_first_word (word:string) :unit =
    
    letters.Clear()
    coordinatesDict.Clear()

    let starting_coordinate = {X=0;Y=0}   
    let coordinate_list_for_the_word = [starting_coordinate]@(moveToCoordinate starting_coordinate (word.Length - 1) ACROSS ToEnd)
    let coordinate_list_for_the_wordAndChar = [for i in 0 .. coordinate_list_for_the_word.Length - 1 -> (coordinate_list_for_the_word.[i] , word.[i])]

    do_dict_updates {word=""; intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=ACROSS}


let TESTING_seed_the_first_word (word:string) (direction:Direction) (starting_coordinate:Coordinate) (clear:string option) :unit =
  
    if clear = Some "clear" then
        letters.Clear()
        coordinatesDict.Clear()

    match direction with

    | ACROSS -> let coordinate_list_for_the_word = [starting_coordinate]@(moveToCoordinate starting_coordinate (word.Length - 1) ACROSS ToEnd)
                let coordinate_list_for_the_wordAndChar = [for i in 0 .. coordinate_list_for_the_word.Length - 1 -> (coordinate_list_for_the_word.[i] , word.[i])]
                do_dict_updates {word=""; intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=ACROSS}

    | DOWN   -> let coordinate_list_for_the_word = [starting_coordinate]@(moveToCoordinate starting_coordinate (word.Length - 1) DOWN ToEnd)
                let coordinate_list_for_the_wordAndChar = [for i in 0 .. coordinate_list_for_the_word.Length - 1 -> (coordinate_list_for_the_word.[i] , word.[i])]
                do_dict_updates {word=""; intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=DOWN}



let rec update_the_dictionaries (source_words:string list) (length_of_previous_failed_list:int) =

    let failed_list =
        source_words 
        |> returns_matching_letters_on_the_grid 
        |> return_status_of_candidate_coordinates 
        |> collect_the_valid_coordinates
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



let helperprintout x y =

     let found, res = coordinatesDict.TryGetValue {X=x;Y=y}

     match found with
     | true -> res.Letter
     | false -> ' '

let lowestX() =

    match coordinatesDict.Count with
    | c when c > 0 -> seq { for kvp in coordinatesDict do yield kvp.Key.X } |> Seq.min
    | _            -> 0

let highestX() =

    match coordinatesDict.Count with
    | c when c > 0 -> seq { for kvp in coordinatesDict do yield kvp.Key.X } |> Seq.max
    | _            -> 0

let lowestY() =

    match coordinatesDict.Count with
    | c when c > 0 -> seq { for kvp in coordinatesDict do yield kvp.Key.Y } |> Seq.min
    | _            -> 0

let highestY() =

    match coordinatesDict.Count with
    | c when c > 0 -> seq { for kvp in coordinatesDict do yield kvp.Key.Y } |> Seq.max
    | _            -> 0

let printBlock() =

    let y_top     = highestY()
    let y_bottom  = lowestY()
    let x_right   = highestX()
    let x_left    = lowestX()

    for y in y_top .. -1 .. y_bottom do

        let row_data = seq { for x in x_left .. x_right do yield (helperprintout x y) }

        let return_a_row = row_data 
                           |> Seq.map(fun a -> a.ToString() )
                           |> Seq.reduce (fun a b -> a + b)

        printfn "%A" return_a_row

let debug b =

   seq { for a in b do
             printfn "debug %A" a
             yield a
       }



TESTING_seed_the_first_word source_words_2.Head ACROSS ({X=0 ; Y=0}) (Some("clear"))
update_the_dictionaries  (source_words_2.Tail |> List.take 200) 0
//update_the_dictionaries  (source_words_2.Tail) 0


//printBlock()

//for kvp in letters         do printfn "Key: %A, Value: %A" kvp.Key kvp.Value.Length
//for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value












