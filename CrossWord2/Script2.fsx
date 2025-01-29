

open System
open System.IO
open System.Collections.Generic


// what if, early on we do not have a matching letter to add the new work to the dictionaries
// we can add the word to a list of words to be processed later

// setup first word
// seq to read from the word list
// seq to return a letter from a word
// find the letter in the Dictionary
// if found add to the Dicts
// if not found flag the Word_start


// create a list of all the word
// new list as a word is placed
// if a word is not placed add to the output list

type word_placing_status = { failed_to_be_placed: string list}


let check_adjacent_cells (letter:char) = true

let find_in_Dict (letters:System.Collections.Generic.IDictionary<char,CoordinateList>) letter =
           let myList = letters.Item(letter)
           match (letters.ContainsKey letter) with
           | true -> let res = check_adjacent_cells letter
                     match res with
                     | true -> true
                     | false -> false
           | false -> false


let place_the_word (word:string) = 
        let wordList = word.ToCharArray() |> Seq.ofArray
        wordList |> Seq.takeWhile (fun c -> find_in_Dict letters c) |> ignore
        true


let place_words_in_the_grid source_words = 
    List.fold (fun acc elem -> match (place_the_word elem) with 
                               | true -> acc
                               | false -> acc@[elem]   ) [] source_words


let rec multiple_passes_words_in_the_grid previous_fail_count source_words = 

        let new_word_list = place_words_in_the_grid source_words
        match (new_word_list.Length) with
        | l when l = 0                             -> l // all words on new list have been placed. End the Fold.
        | l when l <> 0 && l = previous_fail_count -> l // could not add any of the failed list to the grid. Failed list length has not changed. Abort the Fold.
        | l -> multiple_passes_words_in_the_grid l new_word_list













let sequence1 = seq { 1 .. 10 }

let test1 (sequence: int seq) =

    seq {
        for num in sequence do
            printfn("test1 %i") num
            yield num
    }

let can_add num = 
    if num = 15 then false else true

let test2 (sequence: int seq) =

    seq {
        for num in sequence do
             printfn("test2 %i") num
             yield num
    }

let test3 (sequence: int seq) =

    sequence |> Seq.take 4 |> Seq.iter (fun num -> printfn("test3 (take 4): %i") num) |> ignore

    seq {
        for num in sequence do
            printfn("test3 %i") num
            yield num
    }

sequence1 |> test1 |> test2 |> test3 |> Seq.iter (printfn "output %i") |> ignore






let mySequence = seq {
    for i in 1 .. 10 do
        if i > 5 then
            yield! Seq.empty // Exit the sequence early
        else
            yield i
}

// Usage example
mySequence |> Seq.toList |> printfn "%A" // Output: [1; 2; 3; 4; 5]



type Charge =
     | In of int
     | Out of int

let inputs = seq { In 1; Out 2; In 3 }

let ans1 =

    (0, inputs) ||> Seq.scan (fun acc charge ->
         match charge with
         | In i -> acc + i
         | Out o -> acc - o)


let ans2 =

    (0, inputs) ||> Seq.fold (fun acc charge ->
         match charge with
         | In i -> acc + i
         | Out o -> acc - o)

ans1 |> Seq.iter (printfn "%A") 


(*




 for my work the final exit tuple will be  Valid(x,y)  or  NotValid(x,y) or NoValid

 coordinates
 |> Seq.scan
 |> Seq.filter (fun x -> x = Valid(x,y) or NoValid ) 
 |> do the yield

 *)

let scanWithEarlyExit xp initialState sequence =
    sequence
    |> Seq.scan (fun state item -> let newState = fst(state) + item
                                   newState, xp newState             ) initialState
    |> Seq.takeWhile snd
    |> Seq.map fst

let numbers = seq { 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 }
let result = scanWithEarlyExit (fun state -> state < 15) (0,true) numbers

result |> Seq.toList |> printfn "%A"








type Coordinate = { X: int; Y: int }

type Coordinate_status =
| Valid of Coordinate
| NotValid of Coordinate

type Overall_coordinates_status =
| AtleastOneValid
| NoneValid


type State = { this_coordinate: Coordinate_status; overall_status: Overall_coordinates_status }

let random = Random()

let can_add_word_here (c:char) (coordinates:seq<Coordinate>) = 

    let update_overall_status this_coordinate overall_status =

        match (this_coordinate, overall_status) with
        | (Valid(xy), _)                  -> AtleastOneValid
        | (NotValid(xy), AtleastOneValid) -> AtleastOneValid
        | (NotValid(xy), _)               -> overall_status

    coordinates |> 
    Seq.scan (fun state xy -> match xy with
                              | {X=x;Y=y} when x = 10 && y = 10 -> {this_coordinate=Valid(xy)   ; overall_status=update_overall_status (Valid(xy)) state.overall_status }
                              | {X=x;Y=y} when x = 11 && y = 11 -> {this_coordinate=NotValid(xy); overall_status=update_overall_status (NotValid(xy)) state.overall_status }
                              | {X=x;Y=y} when x = 12 && y = 12 -> {this_coordinate=NotValid(xy); overall_status=update_overall_status (NotValid(xy)) state.overall_status }
                              | _ ->                               {this_coordinate=NotValid(xy); overall_status=update_overall_status (NotValid(xy)) state.overall_status }
                              
                                                                 ) {this_coordinate=NotValid({X=0;Y=0}); overall_status=NoneValid}



can_add_word_here 'a' (seq { {X=11;Y=11} ; {X=11;Y=11} ; {X=10;Y=10}} ) 
|> Seq.skip 1
|> Seq.iter (printfn "iter %A") 
|> ignore


can_add_word_here 'a' (seq { {X=11;Y=11} ; {X=11;Y=11} ; {X=10;Y=10}} ) 
|> Seq.skip 1
|> Seq.last
|> printfn "iter %A"
|> ignore




