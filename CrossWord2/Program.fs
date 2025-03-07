
module Program

open System
open System.IO
open System.Threading
open System.Diagnostics

open Module_Common
open Helper
open Module1
open Module2

// https://github.com/dwyl/english-words/blob/master/words_alpha.txt

let source = Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt")

let source_words   = [ "hello"; "world"; "goodbye"; "cruel"; "place" ]

#if DEBUG
let source_words_2 =   System.IO.File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt"))
                           |> Seq.filter (fun w -> w.Length > 1)
                           |> Seq.toList
#else
let source_words_2 =   System.IO.File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt"))
                           |> Seq.filter (fun w -> w.Length > 1)
                           |> Seq.randomShuffle
                           |> Seq.toList
#endif

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

let printText2 state = 
    lock consoleLock (fun () -> printfn "%i]  %s" word_count_1 word_to_print_1 )

let printCount state =
    lock consoleLock (fun () -> printfn "%A %A " word_to_print_2 availableXYcounter_2)

//let timer  = new Timer(printText,  null, 0, 10000)
let timer  = new Timer(printText2, null, 0, 10000)
//let timer2 = new Timer(printCount, null, 0, 10000)

// ==================================================

let random = Random()

let availableXYforWord_action_clear() =

    availableXYforWord.Clear()

let availableXYforWord_action_add counter for_dictionary_update =

    match availableXYforWord.TryAdd(counter,for_dictionary_update) with
    | false -> failwithf "availableXYforWord_action_add %A %A" counter for_dictionary_update; ()
    | true  -> ()

let stats_on_letters_action_clear() =

    stats_on_letter_dictionary.Clear()

let stats_on_letter_dictionary_action_update (letter:char) letter_list_index : unit =

    let found, res = stats_on_letter_dictionary.TryGetValue letter

    match found with
    | true  -> stats_on_letter_dictionary.Item(letter) <- {stats_on_letter_dictionary.running_total_of_letter_dict_indexes=res.running_total_of_letter_dict_indexes + letter_list_index; count_of_letter_dict_indexes = res.count_of_letter_dict_indexes + 1}
    | false -> match (stats_on_letter_dictionary.TryAdd(letter,{stats_on_letter_dictionary.running_total_of_letter_dict_indexes=letter_list_index; count_of_letter_dict_indexes = 1})) with
               | true -> ()
               | false -> failwithf "stats_on_letter_dictionary_action_add_one. Failed to add %A %A" letter letter_list_index
  
let randomXYSelection count = 

#if DEBUG
    let rndKey = 1
#else
    let rndKey = random.Next(1, count + 1)
#endif

    let found , res = availableXYforWord.TryGetValue(rndKey)

    match found with
    | true  -> (res, rndKey)
    | false -> failwithf "randomXYSelection %A" count

let letter_dict_housekeeping() =

    for kvp in stats_on_letter_dictionary do
        
        if kvp.Value.count_of_letter_dict_indexes <> 0 then
           let splitPoint = kvp.Value.running_total_of_letter_dict_indexes / kvp.Value.count_of_letter_dict_indexes
           if splitPoint <> 0 then
              let found , lettersItem = letters.TryGetValue(kvp.Key)
              match found with
              | true -> if not lettersItem.IsEmpty then
                           let (_, newItem) = List.splitAt splitPoint lettersItem
                           if not newItem.IsEmpty then
                              letters.Item(kvp.Key) <- newItem
              | false -> failwithf "letter_dict_housekeeping %A %A" kvp.Key kvp.Value

let collect_the_valid_coordinates_and_select_one_of_them (coordinates:seq<Word_state3>)  =
 
  stats_on_letters_action_clear()

  seq { for coordinate in coordinates do
  
            match coordinate with
            | DATA3 a -> match a.position_on_the_grid with
                         | Some p -> yield (DATA3b { word=a.word; word_count=a.word_count; letter_position=a.letter_position; letter_dict_index=p.letter_dict_index; can_add_word_here=p.can_add_word_here ; for_dictionary_update=p.for_dictionary_update})
                         | None   -> yield! Seq.empty // filtered out earlier
            | MARKER3 b -> yield MARKER3b { end_of_records_marker_for_a_word=b.end_of_records_marker_for_a_word; word_count=b.word_count}
       }

  |> Seq.scan(fun (state:Word_state4) xy -> match xy with
                                            | DATA3b a   -> match state with
                                                            | Final f      -> // previous state was FINAL. So all data records have read for the last block.
                                                                              // this record is DATA. So this is the first record of the next block.
                                                                              availableXYforWord_action_clear()
                                                                              availableXYforWord_action_add 1 a.for_dictionary_update
                                                                              stats_on_letter_dictionary_action_update (a.word[a.letter_position]) a.letter_dict_index
                                                                              Intermediate({word=a.word; word_count=a.word_count; running_total_of_letter_dict_indexes=f.running_total_of_letter_dict_indexes; count_of_letter_dict_indexes=1; availableXYcounter=1})           
                                                            | Intermediate i -> // previous state was Intermediate this record is also DATA. So this is another record in the current block
                                                                              availableXYforWord_action_add (i.availableXYcounter + 1) a.for_dictionary_update
                                                                              stats_on_letter_dictionary_action_update (a.word[a.letter_position]) a.letter_dict_index
                                                                              Intermediate({word=a.word; word_count=a.word_count; running_total_of_letter_dict_indexes=i.running_total_of_letter_dict_indexes + a.letter_dict_index; count_of_letter_dict_indexes=i.count_of_letter_dict_indexes + 1; availableXYcounter=i.availableXYcounter + 1})
                                            | MARKER3b b -> match state with
                                                            | Final f       -> // previous state was FINAL this record is MARKER. 
                                                                              // This means no preceeding DATA records (no valid coodinates) for this current MARKER record.
                                                                              availableXYforWord_action_clear()
                                                                              Final({word=b.end_of_records_marker_for_a_word; word_count=b.word_count; running_total_of_letter_dict_indexes=f.running_total_of_letter_dict_indexes; count_of_letter_dict_indexes=f.count_of_letter_dict_indexes; for_dictionary_update=None})
                                                            | Intermediate i -> // previous state was INTERMEDIATE this record is MARKER.
                                                                              // this means we have read all the DATA records for the current block.
                                                                              // randomly select from the Dictionary one of the valid XY coordinates. To be used for placing the word on the grid.
                                                                              let (selectedCoordinateForDictUpdate, XYoffset) = randomXYSelection i.availableXYcounter
                                                                              availableXYforWord_action_clear()

                                                                              word_to_print_2         <- b.end_of_records_marker_for_a_word
                                                                              availableXYcounter_2    <- i.availableXYcounter

                                                                              if (b.word_count % housekeeping_required = 0) then 
                                                                                  letter_dict_housekeeping()
                                                                                  stats_on_letters_action_clear()
                                                                               
                                                                              Final({word=b.end_of_records_marker_for_a_word; word_count=b.word_count; running_total_of_letter_dict_indexes=i.running_total_of_letter_dict_indexes; count_of_letter_dict_indexes=i.count_of_letter_dict_indexes; for_dictionary_update=Some(selectedCoordinateForDictUpdate)})

    ) (Final {word=""; word_count=0;running_total_of_letter_dict_indexes=0; count_of_letter_dict_indexes=0;for_dictionary_update=None})
  
  |> Seq.skip 1 // omit the initial state

  |> fun c -> seq { for r in c do match r with
                                  | Final f -> yield f
                                  | _       -> yield! Seq.empty }

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

let Update_dictionaries_output_failed_words (dictionary_data:seq<Word_state4_Final>) =

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

let limited_matching_XY_per_word (data:Word_state3_data seq) =

        let return_only_Some_data (data:Word_state3_data seq) = 

            seq { for item in data do 
                   match item.position_on_the_grid with
                   | Some a -> yield item
                   | _      -> yield! Seq.empty
                }

        data
        |> return_only_Some_data
        |> Seq.truncate xy_word_selection_limit

let limited_matching_XY_per_letter (word_count:int, word:string) =

    seq { let availableXY =   

            (word,word_count)
             |> wordAsArray
             |> readThroughLetters
             |> forEachLetterReadThroughCandidateXY

          yield! availableXY

        }

let returns_matching_letters_on_the_grid (source_words:list<string>) : seq<Word_state3> =

        let words_and_word_counts = source_words |> Seq.mapi(fun i word -> (i,word))

        seq {
              for (word_count, word) in words_and_word_counts do

                  word_to_print_1 <- word
                  word_count_this_batch_1 <- (word_count_this_batch_1 + 1)
                  word_count_1 <- word_count_1 + 1

                  let limitedAvailableXY =

                      (word_count, word) 
                      |> limited_matching_XY_per_letter 
                      |> limited_matching_XY_per_word
                      |> Seq.map(fun d -> DATA3(d))
                  
                  yield! limitedAvailableXY 

                  yield MARKER3 {end_of_records_marker_for_a_word=word; word_count=word_count} 
        }

let rec update_the_dictionaries (source_words:string list) (length_of_previous_failed_list:int) =

    let failed_list =
        source_words 
        |> returns_matching_letters_on_the_grid 
        |> collect_the_valid_coordinates_and_select_one_of_them
        |> Update_dictionaries_output_failed_words
        |> Seq.toList

    printfn ("failed_list length %A") failed_list.Length
    printfn ("failed_list %A") failed_list

    match failed_list.Length with
    | l when l = length_of_previous_failed_list && l <> 0 -> failed_list // these words cannot be added.
    | l when l = 0 -> []                                                 // all words have been added.
    | _ -> update_the_dictionaries failed_list failed_list.Length        // retry the list of failed-to-be-added-words

let main() =

    let stopwatch = Stopwatch()
    
    stopwatch.Start()
    
    TESTING_seed_the_first_word source_words_2.Head ACROSS ({X=0 ; Y=0}) (Some("clear"))
#if DEBUG
    update_the_dictionaries  (source_words_2.Tail |> List.take 2000) 0 |> ignore
#else
    update_the_dictionaries  (source_words_2.Tail) 0 |> ignore
#endif

    stopwatch.Stop()

    let elapsed = stopwatch.Elapsed
    let formattedTime = sprintf "%02d:%02d:%02d" elapsed.Hours elapsed.Minutes elapsed.Seconds
    printfn "Elapsed time: %s" formattedTime

    timer.Dispose()  |> ignore

  //timer2.Dispose() |> ignore
  //Console.ReadLine() |> ignore
 



main()
writeGrid_to_file()













