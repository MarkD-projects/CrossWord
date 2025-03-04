
open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Diagnostics

// https://github.com/dwyl/english-words/blob/master/words_alpha.txt

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

type For_dictionary_update = {word:string; intersection_coordinate:Coordinate ; coordinates_of_the_word:(Coordinate*char) seq ; new_word_direction:Direction}

let letters = Dictionary<char , Coordinate list>()

//		there will be a Coordinate 					Dictionary of Key (x,y)    	and Value ( {letter, Down=Some(Placed\Indirect) or None, Up=Some(Placed\Indirect) or None } )
let coordinatesDict = Dictionary<Coordinate , Letter_info>()

//      available xy for current word
let availableXYforWord = Dictionary<int,For_dictionary_update>()

//		there will be a Grid Placed Word			Dictionary of Key (word)   	and Value ({starting position (x,y), direction}).		
let grid_placed_words = dict ([] : (string * Word_start) list)


//		there will be a Grid Indirect Word 			Dictionary of Key (word) 	and Value (list of records of {starting position (x,y), direction}).
let grid_indirect_words = dict ([] : (string * Word_start) list)

//		there will be a Grid Indirect Invalid Word 	Dictionary of Key (word) 	and Value (list of records of {starting position (x,y), direction}).
let grid_indirect_invalid_words = dict ([] : (string * Word_start) list)


//type State = { this_coordinate: Coordinate_status; overall_status: Overall_coordinates_status ; for_dictionary_update: For_dictionary_update option }
//type State = { this_coordinate: Coordinate_status; for_dictionary_update: For_dictionary_update option }

type Position_on_the_grid = {can_add_word_here: Coordinate; for_dictionary_update: For_dictionary_update; letter_dict_index:int} 

//type Word_state2_data   = { word: string; letter_position: int; candidate_Coordinate: Coordinate option; for_dictionary_update: For_dictionary_update option }
//type Word_state2_marker = { end_of_records_marker_for_a_word: string}
//type Word_state2 =
//| DATA2   of Word_state2_data
//| MARKER2 of Word_state2_marker


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

//type AccStatus =
//| Final
//| Intermediate

type Word_state4_Intermediate = { word:string;  word_count:int; total_of_letter_dict_index:int; availableXYcounter:int; letter_dict_index:int }
type Word_state4_Final        = { word:string;  word_count:int; total_of_letter_dict_index:int; for_dictionary_update: For_dictionary_update option }
type Word_state4 =
| Intermediate of Word_state4_Intermediate
| Final        of Word_state4_Final


type Word_state5  = { word:string; word_count:int; for_dictionary_update: For_dictionary_update option }
//type Word_state6  = { word:string; for_dictionary_update: For_dictionary_update option }

type failed_list = seq<string>

// DEBUG ============================================
let consoleLock = obj()
let mutable word_count_1 = 0
let mutable word_to_print_1 = ""
let mutable word_count_last_batch_1 = 0
let mutable word_count_this_batch_1 = 0

let mutable word_to_print_2 = ""
let mutable availableXYcounter_2 = 0

let printText state = 
    lock consoleLock (fun () -> printfn "%-10i] %-20s  %-10i" word_count_1 word_to_print_1 word_count_this_batch_1)
    word_count_last_batch_1 <- word_count_this_batch_1
    word_count_this_batch_1 <- 0

let printCount state =
    lock consoleLock (fun () -> printfn "%A %A " word_to_print_2 availableXYcounter_2)

let timer  = new Timer(printText,  null, 0, 5000)
let timer2 = new Timer(printCount, null, 0, 5000)
// ==================================================

let xy_letter_selection_limit = 5
let xy_word_selection_limit = xy_letter_selection_limit * 10
let housekeeping_required = 100

let random = Random()

let isCellEmpty (coordinate:Coordinate) = not (coordinatesDict.ContainsKey coordinate)

//let isCellAvailiable (coordinate:Coordinate) (c:char) (matchType:MatchType) = 

//     let found, res = coordinatesDict.TryGetValue coordinate

//     match (matchType  , found) with
//     | Letter          , true  when res.Letter = c  -> true
//     | Letter          , true  when res.Letter <> c -> false
//     | Letter          , false                      -> false
//     | LetterOrEmpty   , true  when res.Letter = c  -> true
//     | LetterOrEmpty   , true  when res.Letter <> c -> false
//     | LetterOrEmpty   , false                      -> true
//     | MatchType.Empty , false                      -> true
//     | _                                            -> false

let directionForWordToBePlaced (res:Letter_info) =

     match (res.Down , res.Across) with
     | Some x , None   -> Some(ACROSS)  // select right-angles direction to existing word
     | None   , Some x -> Some(DOWN)
     | _               -> None

let cellStatus (coordinate:Coordinate) (c:char) = 

     let found, res = coordinatesDict.TryGetValue coordinate

     match found with
     | true  when res.Letter = c  -> {cellContent=MatchingLetter    ; availableDirection=directionForWordToBePlaced res}
     | true  when res.Letter <> c -> {cellContent=NoMatchingLetter  ; availableDirection=directionForWordToBePlaced res}
     | _                          -> {cellContent=CellContent.Empty ; availableDirection=None}

let moveToCoordinates start cellCountToMove lineOfTheWord directionOfMovement =

    match cellCountToMove with
    | 0 -> Seq.empty
    | _ ->  match (lineOfTheWord , directionOfMovement) with
            | ACROSS , ToStart -> seq { for i in cellCountToMove .. -1 .. 1 do yield { start with X = start.X - i} }
            | ACROSS , ToEnd   -> seq { for i in 1 .. cellCountToMove       do yield { start with X = start.X + i} }
            | DOWN   , ToStart -> seq { for i in cellCountToMove .. -1 .. 1 do yield { start with Y = start.Y + i} }
            | DOWN   , ToEnd   -> seq { for i in 1 .. cellCountToMove       do yield { start with Y = start.Y - i} } 

let returnAdjacentCellsXY (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate) =

    match lineOfTheWordToBeAdded with
    | ACROSS -> seq { yield {X=gridCoordinate.X;     Y=gridCoordinate.Y + 1} ; yield {X=gridCoordinate.X     ; Y=gridCoordinate.Y - 1} }
    | DOWN   -> seq { yield {X=gridCoordinate.X - 1; Y=gridCoordinate.Y}     ; yield {X=gridCoordinate.X + 1 ; Y=gridCoordinate.Y}     }

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

    returnAdjacentCellsXY lineOfTheWordToBeAdded gridCoordinate |> Seq.forall( fun xy -> isCellEmpty xy )

let checkAvailabilityOfRemainingCells (word:string) (wordsplit:WordSplit) (lineOfTheWordToBeAdded:Direction) (gridCoordinate:Coordinate) =

    // Note using position not offset in the movement calculations

    let coordinateAdjacentToStartLetter() = Seq.head (moveToCoordinates gridCoordinate (wordsplit.NumberOflettersBeforeTheIntersectionLetter + 1) lineOfTheWordToBeAdded ToStart)
    let coordinateAdjacentToEndLetter()   = Seq.head (moveToCoordinates gridCoordinate (wordsplit.NumberOflettersAfterTheIntersectionLetter  + 1) lineOfTheWordToBeAdded ToEnd  )

    let coordinatesStartUpToIntersectingLetter()  = moveToCoordinates gridCoordinate wordsplit.NumberOflettersBeforeTheIntersectionLetter lineOfTheWordToBeAdded ToStart
    let coordinatesAfterIntersectingToEndLetter() = moveToCoordinates gridCoordinate wordsplit.NumberOflettersAfterTheIntersectionLetter  lineOfTheWordToBeAdded ToEnd

    let coordinatesStartUpToIntersectingLetterAndChar()  = coordinatesStartUpToIntersectingLetter()  |> Seq.mapi (fun i coordinate -> (coordinate , word.[i]) )
    let coordinatesAfterIntersectingToEndLetterAndChar() = coordinatesAfterIntersectingToEndLetter() |> Seq.mapi (fun i coordinate -> (coordinate , word.[wordsplit.offsetOfIntersectingLetter + 1 + i]) )

    let allCoordinates() = Seq.append (coordinatesStartUpToIntersectingLetterAndChar()) (coordinatesAfterIntersectingToEndLetterAndChar())
                           |> Seq.cache

    let isCellAvailable (xy,c) =

        match (cellStatus xy c) with                                  
        |{cellContent=MatchingLetter} -> true   
        |{cellContent=Empty         } -> no_adjacent_word_to_the_added_letter lineOfTheWordToBeAdded xy
        |_                            -> false

    let allCellsAvailable allCoordinates = 
        allCoordinates |> Seq.forall (fun coorAndChar -> isCellAvailable coorAndChar)

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

          let allCoordinates = allCoordinates() // cache

          if allCellsAvailable allCoordinates then

             Some( {word=word;
                    intersection_coordinate=gridCoordinate;
                    coordinates_of_the_word=Seq.append allCoordinates (seq { yield (gridCoordinate,word.[wordsplit.offsetOfIntersectingLetter]) });
                    new_word_direction=lineOfTheWordToBeAdded} )    
  
          else

             None
     
       else

          None 

    else

       None

let areCellsAvailiable (word:string) (wordsplit:WordSplit) (gridCoordinate:Coordinate) =

    // the first check will be the intersection letter taken from the Letters Dictionary.
    // that letter will be at that XY stored in the Coordinates Dictionary.
    // The Coordinates Dictionary will indicate if that letter can be used.
    
    match (cellStatus gridCoordinate word.[wordsplit.offsetOfIntersectingLetter]) with                                  
    |{cellContent=MatchingLetter; availableDirection=Some(x)} -> checkAvailabilityOfRemainingCells word wordsplit x gridCoordinate
    |{cellContent=MatchingLetter; availableDirection=None}    -> None
    |_                                                        -> failwithf "In areCellsAvailiable the call to cellStatus fails with parameters %A %A " gridCoordinate word.[wordsplit.offsetOfIntersectingLetter] ; None

//let return_status_of_candidate_coordinates (coordinates:seq<Word_state3>) : seq<Word_state3>  =

//    coordinates
//    |> Seq.scan (fun state xy -> match xy with
//                                 | DATA3 a -> match a.candidate_Coordinate with
//                                              | None   -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=None }
//                                            //| Some c -> let here = areCellsAvailiable a.word a.letter_position c
//                                            //            match here with
//                                            //            | Some h -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=Some{can_add_word_here=c; for_dictionary_update=h} }
//                                            //            | None   -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=None }
//                                              | Some c -> DATA3 { word=a.word; letter_position=a.letter_position; position_on_the_grid=Some{can_add_word_here=a.candidate_Coordinate; for_dictionary_update=a.for_dictionary_update} }


//                                 | MARKER3 b -> MARKER3 { end_of_records_marker_for_a_word=b.end_of_records_marker_for_a_word}
    
//    ) (DATA3 { word="";letter_position=0; position_on_the_grid=None})
//    |> Seq.skip 1 // omit the initial state

let availableXYforWord_action_clear() =

    availableXYforWord.Clear()

let availableXYforWord_action_add counter for_dictionary_update =

    match availableXYforWord.TryAdd(counter,for_dictionary_update) with
    | false -> failwithf "availableXYforWord_action_add %A %A" counter for_dictionary_update; ()
    | true  -> ()

let randomXYSelection count = 

    let rndKey = random.Next(1, count + 1)

    let found , res = availableXYforWord.TryGetValue(rndKey)

    match found with
    | true  -> (res, rndKey)
    | false -> failwithf "randomXYSelection %A" count

let collect_the_valid_coordinates_and_select_one_of_them (coordinates:seq<Word_state3>)  =
 
  seq { for coordinate in coordinates do
  
            match coordinate with
            | DATA3 a -> match a.position_on_the_grid with
                         | Some p -> yield (DATA3b { word=a.word; word_count=a.word_count; letter_position=a.letter_position; letter_dict_index=p.letter_dict_index; can_add_word_here=p.can_add_word_here ; for_dictionary_update=p.for_dictionary_update})
                         | None   -> yield! Seq.empty
            | MARKER3 b -> yield MARKER3b { end_of_records_marker_for_a_word=b.end_of_records_marker_for_a_word; word_count=b.word_count}
       }

  |> Seq.scan(fun (state:Word_state4) xy -> match xy with
                                            | DATA3b a   -> match state with
                                                            | Final f      -> // previous state was FINAL. So all data records have read for the last block.
                                                                              // this record is DATA. So this is the first record of the next block.
                                                                              availableXYforWord_action_clear()
                                                                              availableXYforWord_action_add 1 a.for_dictionary_update
                                                                              Intermediate({word=a.word; word_count=a.word_count; availableXYcounter=1; letter_dict_index=a.letter_dict_index})           
                                                            | Intermediate i -> // previous state was Intermediate this record is also DATA. So this is another record in the current block
                                                                              availableXYforWord_action_add (i.availableXYcounter + 1) a.for_dictionary_update
                                                                              Intermediate({word=a.word; word_count=a.word_count; availableXYcounter=i.availableXYcounter + 1; letter_dict_index=i.letter_dict_index + a.letter_dict_index})
                                            | MARKER3b b -> match state with
                                                            | Final f       -> // previous state was FINAL this record is MARKER. 
                                                                              // This means no preceeding DATA records (no valid coodinates) for this current MARKER record.
                                                                              availableXYforWord_action_clear()
                                                                              Final({word=b.end_of_records_marker_for_a_word; word_count=b.word_count; total_of_letter_dict_index=0; for_dictionary_update=None})
                                                            | Intermediate i -> // previous state was INTERMEDIATE this record is MARKER.
                                                                              // this means we have read all the DATA records for the current block.
                                                                              // randomly select from the Dictionary one of the valid XY coordinates. To be used for placing the word on the grid.
                                                                              let (selectedCoordinateForDictUpdate, XYoffset) = randomXYSelection i.availableXYcounter
                                                                              availableXYforWord_action_clear()

                                                                              word_to_print_2         <- b.end_of_records_marker_for_a_word
                                                                              availableXYcounter_2    <- i.availableXYcounter

                                                                              Final({word=b.end_of_records_marker_for_a_word; word_count=b.word_count; total_of_letter_dict_index=0; for_dictionary_update=Some(selectedCoordinateForDictUpdate)})

    ) (Final {word=""; word_count=0;total_of_letter_dict_index=0;for_dictionary_update=None})
  
  |> Seq.skip 1 // omit the initial state

  |> fun c -> seq { for r in c do match r with
                                  | Final f -> yield f
                                  | _       -> yield! Seq.empty }

  |> Seq.map(fun c -> {Word_state5.word=c.word; word_count=c.word_count; for_dictionary_update = c.for_dictionary_update} )

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

let Update_dictionaries_output_failed_words (dictionary_data:seq<Word_state5>) =
    // printfn "Update_dictionaries_output_failed_words"
    seq {
        for c in dictionary_data do
            match c.for_dictionary_update with 
            | Some coordinate_info -> do_dict_updates coordinate_info
                                      if (c.word_count % housekeeping_required) = 0 then letter_dict_housekeeping c.
                                      yield! Seq.empty
            | _                    -> yield c.word
    }

let seed_the_first_word (word:string) :unit =
    
    letters.Clear()
    coordinatesDict.Clear()

    let starting_coordinate = {X=0;Y=0}   
    let coordinate_list_for_the_word        = seq { yield starting_coordinate ; yield! (moveToCoordinates starting_coordinate (word.Length - 1) ACROSS ToEnd) }
    let coordinate_list_for_the_wordAndChar = coordinate_list_for_the_word |> Seq.mapi (fun i coor -> (coor, word.[i]))

    do_dict_updates {word=""; intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=ACROSS}


let TESTING_seed_the_first_word (word:string) (direction:Direction) (starting_coordinate:Coordinate) (clear:string option) :unit =
  
    if clear = Some "clear" then
        letters.Clear()
        coordinatesDict.Clear()

    match direction with

    | ACROSS -> let coordinate_list_for_the_word = seq { yield starting_coordinate ; yield! (moveToCoordinates starting_coordinate (word.Length - 1) ACROSS ToEnd) }
                let coordinate_list_for_the_wordAndChar = coordinate_list_for_the_word |> Seq.mapi (fun i coor -> (coor, word.[i]))
                do_dict_updates {word=""; intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=ACROSS}

    | DOWN   -> let coordinate_list_for_the_word = seq { yield starting_coordinate ; yield! (moveToCoordinates starting_coordinate (word.Length - 1) DOWN ToEnd) }
                let coordinate_list_for_the_wordAndChar = coordinate_list_for_the_word |> Seq.mapi (fun i coor -> (coor, word.[i]))
                do_dict_updates {word=""; intersection_coordinate=starting_coordinate; coordinates_of_the_word=coordinate_list_for_the_wordAndChar ; new_word_direction=DOWN}

let letter_dict_housekeeping() = ()

let limit_matching_XY_per_word (data:Word_state3 seq) =

        data 

        |> Seq.scan (fun (data,count) x -> match x with
                                           | DATA3 a -> match a.position_on_the_grid with
                                                        | Some a -> match count with
                                                                    | count when count < xy_word_selection_limit -> ( Some x , (count + 1) )
                                                                    | _                                     -> ( None   , count       ) // no more candidate xy for a word
                                                        | None   -> ( Some x , count)
                                           | _       -> ( None , count ) )   (None , 0)

        |> Seq.map (fun (data,count) -> data)

        |> Seq.choose id

let limit_matching_XY_per_letter (word_count:int, word:string) =

    seq {
                let wordAsArray = word.ToCharArray()
                let wordLength = word.Length

                for i = 0 to wordAsArray.Length - 1 do
                        let found, res1 = letters.TryGetValue word.[i]
                        match found with
                        | true -> let letterPOSITION = i + 1
                                  let wordsplit = {offsetOfIntersectingLetter=i; positionOfIntersectingLetter=letterPOSITION; NumberOflettersBeforeTheIntersectionLetter=letterPOSITION - 1; NumberOflettersAfterTheIntersectionLetter=wordLength - letterPOSITION}
                                  let ww = res1 |> Seq.mapi(fun i xy -> (i, xy) )
                                  let xx = seq { for (letter_index,xy) in ww do let here = areCellsAvailiable word wordsplit xy
                                                                                match here with 
                                                                                | Some h -> yield DATA3 { word=word; word_count=word_count; letter_position=i; position_on_the_grid=Some {can_add_word_here=xy; letter_dict_index=letter_index; for_dictionary_update=h} }
                                                                                | None   -> ()
                                               } |> Seq.truncate xy_letter_selection_limit
                                  let yy = Seq.cache xx
                                  match Seq.isEmpty yy with
                                  | true  -> yield DATA3 { word=word; word_count=word_count; letter_position=i; position_on_the_grid=None }
                                  | false -> yield! yy

                        | _    -> yield DATA3 { word=word; word_count=word_count; letter_position=i; position_on_the_grid=None }
        }

let returns_matching_letters_on_the_grid (source_words:list<string>) : seq<Word_state3> =

        let words_and_word_counts = source_words |> Seq.mapi(fun i word -> (i,word))

        seq {
              for (word_count, word) in words_and_word_counts do

                  word_to_print_1 <- word
                  word_count_this_batch_1 <- (word_count_this_batch_1 + 1)
                  word_count_1 <- word_count_1 + 1

                  yield! ( (word_count, word) |> limit_matching_XY_per_letter |> limit_matching_XY_per_word)

                  yield MARKER3 {end_of_records_marker_for_a_word=word; word_count=word_count} 
        }

    // >>> WORKING HERE


let rec update_the_dictionaries (source_words:string list) (length_of_previous_failed_list:int) =

    let failed_list =
        source_words 
        |> returns_matching_letters_on_the_grid 
        |> collect_the_valid_coordinates_and_select_one_of_them
        |> Update_dictionaries_output_failed_words
        |> Seq.toList

    printfn ("failed_list length ========================================= %A") failed_list.Length
    printfn ("failed_list %A") failed_list

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

let main() =

    TESTING_seed_the_first_word source_words_2.Head ACROSS ({X=0 ; Y=0}) (Some("clear"))
    //update_the_dictionaries  (source_words_2.Tail |> List.take 4000) 0 |> ignore
    update_the_dictionaries  (source_words_2.Tail) 0 |> ignore
    printfn "========== END =================="
    timer.Dispose()  |> ignore
    timer2.Dispose() |> ignore
  //Console.ReadLine() |> ignore
 

main()
//printBlock()

//for kvp in letters         do printfn "Key: %A, Value: %A" kvp.Key kvp.Value.Length
//for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value












