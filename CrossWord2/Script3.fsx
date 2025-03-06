

open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Diagnostics


#load "Program.fs"
open Program



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

let addIndexToWord (coor:Coordinate list) = 

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


let forEachLetterReturnCandidateXY (word_info:{|word:string;wordAsArray:char array;wordLength:int;res:Coordinate list;letterPosition:int;word_count:int|} )=
     
    let letterPOSITION = word_info.letterPosition + 1
    let wordsplit = {offsetOfIntersectingLetter=word_info.letterPosition; positionOfIntersectingLetter=letterPOSITION; NumberOflettersBeforeTheIntersectionLetter=letterPOSITION - 1; NumberOflettersAfterTheIntersectionLetter=word_info.wordLength - letterPOSITION}

    let candidate_results =

        word_info.res
        |> addIndexToWord
        |> availableXY word_info wordsplit
                        
    let limited_candidate_results = candidate_results |> Seq.truncate xy_letter_selection_limit |> Seq.cache

    match Seq.isEmpty limited_candidate_results with
    | true  -> seq { DATA3 { word=word_info.word; word_count=word_info.word_count; letter_position=word_info.letterPosition; position_on_the_grid=None } }
    | false -> limited_candidate_results


let forEachLetterFindCandidateXY (word_info:{|word:string;wordAsArray:char array;wordLength:int;res:Coordinate list option;letterPosition:int;word_count:int|} seq) =

    seq { for data in word_info do

              match data.res with
              | Some xy -> yield! forEachLetterReturnCandidateXY {|word=data.word;wordAsArray=data.wordAsArray;wordLength=data.wordLength;res=xy;letterPosition=data.letterPosition;word_count=data.word_count|}
              | None    -> yield (DATA3 { word=data.word; word_count=data.word_count; letter_position=data.letterPosition; position_on_the_grid=None })
       }


let limit_matching_XY_per_letter (word_count:int, word:string) =

    seq { let candidateXY =   

            (word,word_count)
             |> wordAsArray
             |> readThroughLetters
             |> forEachLetterFindCandidateXY

          yield candidateXY

        }






















