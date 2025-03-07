

open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Diagnostics

#load "Module_Common.fs"
#load "Helper.fs"
#load "Module2.fs"
#load "Module1.fs"
#load "Program.fs"

open Program
open Module_Common

let (|Case1|Case2|Case3|Case4|) (input: int) =
    match input with
    | 1 -> Choice1Of4 ()
    | 2 -> Choice2Of4 ()
    | 3 -> Choice3Of4 ()
    | _ -> Choice4Of4 ()

let testPattern input =
    match input with
    | Case1 -> printfn "Matched Case1"
    | Case2 -> printfn "Matched Case2"
    | Case3 -> printfn "Matched Case3"
    | Case4 -> printfn "Matched Case4"

testPattern 1  // Output: Matched Case1
testPattern 2  // Output: Matched Case2
testPattern 3  // Output: Matched Case3
testPattern 4  // Output: Matched Case4


let if_there_are indexes_for_this_letter (kvp:KeyValuePair<char,stats_on_letter_dictionary>) = (kvp.Value.count_of_letter_dict_indexes <> 0)
let calculate_the_average_index_of_the_xy_used_in_the_last_xxx_records (kvp:KeyValuePair<char,stats_on_letter_dictionary>) = kvp.Value.running_total_of_letter_dict_indexes / kvp.Value.count_of_letter_dict_indexes
let if_the_average_index_is_not_zero = (calculate_the_average_index_of_the_xy_used_in_the_last_xxx_records <> 0) 

let xx = 

    for kvp in stats_on_letter_dictionary do
 
        let there_are_indexes_for_this_letter = (kvp.Value.count_of_letter_dict_indexes <> 0)
        let calculate_the_average_index_of_the_xy_used_in_the_last_xxx_records = kvp.Value.running_total_of_letter_dict_indexes / kvp.Value.count_of_letter_dict_indexes
        let the_average_index_is_not_zero = (calculate_the_average_index_of_the_xy_used_in_the_last_xxx_records <> 0)  


        if there_are_indexes_for_this_letter then
           if the_average_index_is_not_zero then
              let found , lettersItem = letters.TryGetValue(kvp.Key)
              match found with
              | true -> if not lettersItem.IsEmpty then
                           let (_, newItem) = List.splitAt splitPoint lettersItem
                           if not newItem.IsEmpty then
                              letters.Item(kvp.Key) <- newItem
              | false -> failwithf "letter_dict_housekeeping %A %A" kvp.Key kvp.Value



// if there are indexes for this letter 
// calculate the average index of the xy used in the last 100 records
// use that average to split the list of letters.
// into old in-active xy and recent xy
// update the letters dictionary
















let (|NEW_WORD_BLOCK|END_OF_WORD_BLOCK|MARKER_AFTER_WORD_BLOCK|MARKER_AFTER_EMPTY_WORD_BLOCK|)( xy:Word_state3b, state:Word_state4) : Choice<unit,unit,unit,unit> =
 
    match xy,state with
    | DATA3b a   , Final b        -> NEW_WORD_BLOCK
    | DATA3b a   , Intermediate b -> END_OF_WORD_BLOCK
    | MARKER3b a , Final b        -> NEW_WORD_BLOCK
    | MARKER3b a , Intermediate b -> END_OF_WORD_BLOCK

let test =

 Seq.scan(fun (state:Word_state4) xy -> match (xy,state) with
                                        | NEW_WORD_BLOCK a f ->                // previous state was FINAL. So all data records have read for the last block.
                                                                               // this record is DATA. So this is the first record of the next block.
                                                                               availableXYforWord_action_clear()
                                                                               availableXYforWord_action_add 1 a.for_dictionary_update
                                                                               stats_on_letter_dictionary_action_update (a.word[a.letter_position]) a.letter_dict_index
                                                                               Intermediate({word=a.word; word_count=a.word_count; running_total_of_letter_dict_indexes=f.running_total_of_letter_dict_indexes; count_of_letter_dict_indexes=1; availableXYcounter=1})           
                                        | END_OF_WORD_BLOCK a i ->             // previous state was Intermediate this record is also DATA. So this is another record in the current block
                                                                               availableXYforWord_action_add (i.availableXYcounter + 1) a.for_dictionary_update
                                                                               stats_on_letter_dictionary_action_update (a.word[a.letter_position]) a.letter_dict_index
                                                                               Intermediate({word=a.word; word_count=a.word_count; running_total_of_letter_dict_indexes=i.running_total_of_letter_dict_indexes + a.letter_dict_index; count_of_letter_dict_indexes=i.count_of_letter_dict_indexes + 1; availableXYcounter=i.availableXYcounter + 1})
                                        | MARKER_AFTER_EMPTY_WORD_BLOCK b f->  // previous state was FINAL this record is MARKER. 
                                                                               // This means no preceeding DATA records (no valid coodinates) for this current MARKER record.
                                                                               availableXYforWord_action_clear()
                                                                               Final({word=b.end_of_records_marker_for_a_word; word_count=b.word_count; running_total_of_letter_dict_indexes=f.running_total_of_letter_dict_indexes; count_of_letter_dict_indexes=f.count_of_letter_dict_indexes; for_dictionary_update=None})
                                        | MARKER_AFTER_WORD_BLOCK b i       -> // previous state was INTERMEDIATE this record is MARKER.
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










   
   






let wordAsArray (word:string, word_count:int) =

   let wordAsArray = word.ToCharArray()
   let wordLength  = word.Length

   {|word=word;wordAsArray=wordAsArray;wordLength=wordLength;word_count=word_count|}

let readThroughLetters (word_info:{|word:string;wordAsArray:char array;wordLength:int;word_count:int|}) =

    seq { for i = 0 to word_info.wordLength - 1 do

              let found, res1 = letters.TryGetValue word_info.word.[i]

              match found with
              | true  -> yield {|word=word_info.word;wordAsArray=word_info.wordAsArray;wordLength=word_info.wordLength;res=Some res1;letterPosition=i;word_count=word_info.word_count|}
              | false -> yield {|word=word_info.word;wordAsArray=word_info.wordAsArray;wordLength=word_info.wordLength;res=None;letterPosition=i;word_count=word_info.word_count|}

        }

let addIndexToCoordinate_list (coor:Coordinate list) = 

    coor |> Seq.mapi(fun i xy -> (i, xy) )

let availableXY (word_info:{|word:string;wordAsArray:char array;wordLength:int;res:Coordinate list;letterPosition:int;word_count:int|} ) 
                (wordsplit:WordSplit) 
                (candiate_coordinates:(int*Coordinate) seq) =

        seq { for (letter_dict_index,xy) in candiate_coordinates do 
                                let here = areCellsAvailiable word_info.word wordsplit xy
                                match here with 
                                | Some h -> yield DATA3 { word=word_info.word; word_count=word_info.word_count; letter_position=word_info.letterPosition; position_on_the_grid=Some {can_add_word_here=xy; letter_dict_index=letter_dict_index; for_dictionary_update=h} }
                                | None   -> ()
            }


let forEachLetterReturnValidXY (word_info:{|word:string;wordAsArray:char array;wordLength:int;res:Coordinate list;letterPosition:int;word_count:int|} )=
     
    let letterPOSITION = word_info.letterPosition + 1
    let wordsplit      = {offsetOfIntersectingLetter=word_info.letterPosition; positionOfIntersectingLetter=letterPOSITION; NumberOflettersBeforeTheIntersectionLetter=letterPOSITION - 1; NumberOflettersAfterTheIntersectionLetter=word_info.wordLength - letterPOSITION}

    let limited_candidate_results =

        word_info.res
        |> addIndexToCoordinate_list
        |> availableXY word_info wordsplit
        |> Seq.truncate xy_letter_selection_limit
        |> Seq.cache

    match Seq.isEmpty limited_candidate_results with
    | true  -> seq { DATA3 { word=word_info.word; word_count=word_info.word_count; letter_position=word_info.letterPosition; position_on_the_grid=None } }
    | false -> limited_candidate_results


let forEachLetterReadThroughCandidateXY (word_info:{|word:string;wordAsArray:char array;wordLength:int;res:Coordinate list option;letterPosition:int;word_count:int|} seq) =

    seq { for data in word_info do

              match data.res with
              | Some xy -> yield! forEachLetterReturnValidXY {|word=data.word;wordAsArray=data.wordAsArray;wordLength=data.wordLength;res=xy;letterPosition=data.letterPosition;word_count=data.word_count|}
              | None    -> yield (DATA3 { word=data.word; word_count=data.word_count; letter_position=data.letterPosition; position_on_the_grid=None })
       }

let limited_matching_XY_per_letter (word_count:int, word:string) =

    seq { let availableXY =   

            (word,word_count)
             |> wordAsArray
             |> readThroughLetters
             |> forEachLetterReadThroughCandidateXY

          yield availableXY

        }
























