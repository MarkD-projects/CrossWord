

open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Diagnostics


#load "Program.fs"
open Program


let stopwatch = Stopwatch()

type Coordinate = { X: int; Y: int }

let random = Random()

let dictXY  = Dictionary<Coordinate , uint>()
let dictNum = Dictionary<uint       , uint>()

let test1() =
 dictXY.Clear()
 for i in 1 .. 10000 do dictXY.TryAdd( {X=random.Next(0, 5000) ; Y=random.Next(0, 5000)} , random.Next(0, 5000) |> uint) |> ignore

stopwatch.Start()
test1()
stopwatch.Stop()

printfn "%A" dictXY.Count
printfn "%A" stopwatch.Elapsed


let test2() =
 dictNum.Clear()
 for i in 1 .. 10000 do dictNum.TryAdd( ((random.Next(0, 5000) * 1000000) + random.Next(0, 5000)) |> uint, random.Next(0, 5000) |> uint) |> ignore

stopwatch.Start()
test2()
stopwatch.Stop()

printfn "%A" dictNum.Count
printfn "%A" stopwatch.Elapsed




0 % 100




//["aaaaa";"bbbb";"ccc"]
System.IO.File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt"))
|> Seq.map (fun w -> seq {for c in w.ToCharArray() do yield c})
|> Seq.collect (fun c -> c)
|> Seq.countBy (fun c -> c)
|> Seq.sortBy(fun (_,i) -> i )
|> Seq.iter(fun c -> printfn "%A" c)






//["aaaaa";"bbbb";"ccc"]
System.IO.File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "words_alpha.txt"))
|> Seq.map (fun w -> w.Length |> float)
|> Seq.average







Seq.append [1; 2] [3; 4]
Seq.append (seq {yield 1 ; yield 2}) (seq {yield 3 ; yield 4})
Seq.append (Seq.empty) (seq {yield 3 ; yield 4})


// seq {yield 1 ; yield 2 ; yield 3 ; yield 4}
Seq.empty
|> Seq.mapi (fun i coordinate ->  printfn "%A %A" i coordinate )
|> Seq.isEmpty



let xx = seq { yield 1 ; yield 2 ;yield 3 ;yield 4 ;yield 5 ;yield 6}
seq { for x in xx do
       match x with
       | x when x < 3 || x > 4 -> yield x
       | x -> ()
    }
|> Seq.iter(fun x -> printfn "%A" x)






















let printText state  =  printfn "xxxx %A" state

let timer = new Timer(printText, null, 0, 10000)


printfn "%-20s %-10i %-10i " "aaaaaa" 24 25


let alist = ["aa";"bb";"cc";"dd";"ee"]

let top_level_read alist =

    seq { for a in alist do
           printfn "top level %A" a
           yield a
        }


let second_level alist =

    seq { for a in alist do
            printfn "second level %A" a
            for b in a do
                printfn "second level %A" b
                yield b
        }

alist
|> top_level_read
|> second_level
|> Seq.iter(fun c -> printfn "iter %A" c)



seq { for x in 1 .. 10 do yield x }
|> Seq.scan (fun state num -> match (num % 2) with
                              | 0 -> seq {yield num; yield num * 10}   // sequence of  sequence   <<<<<<<<
                              | _ -> seq {yield num}  ) (seq{yield 0})
|> Seq.collect(fun x -> x)
|> fun x -> Seq.append x [999]   // note, the last record state from the scan will contain all the final info. So can be used to append a marker record if needed.
|> Seq.iter(fun num -> printfn "%A" num)


// NOTE 
// can output an empty sequence. So may be able to omit the No Valid Coordinate records.
// current Scan code does not accumutate the Valid(xy)
// remember output will include the initial record

seq { for x in 1 .. 10 do yield x }
|> Seq.scan (fun state num -> match (num % 2) with
                              | 0 -> Seq.empty     // <<<<<<<<<<
                              | _ -> seq {yield num}  ) (seq{yield 0})
|> Seq.collect(fun x -> x)   // removes the empty sequences
|> fun x -> Seq.append x [999]
|> Seq.iter(fun num -> printfn "%A" num)



seq { for x in 1 .. 10 do yield x }
|> Seq.scan (fun state num -> match (num % 2) with
                              | 0 -> Some(num)
                              | _ -> None ) (Some(0))
|> Seq.iter(fun num -> printfn "%A" num)


seq { for x in 1 .. 10 do 
       if x <> 5 then yield x else yield! Seq.empty      
}
|> Seq.iter(fun num -> printfn "%A" num)


seq { for x in 1 .. 10 do yield x }
|> Seq.scan (fun state num -> match (num % 2) with
                              | 0 -> Seq.empty     // will output empty sequences
                              | _ -> seq {yield num}  ) (seq{yield 0})
|> Seq.iter(fun num -> printfn "%A" num)



seq {yield 'a'; yield 'b'; yield 'c'; yield 'd'; yield 'e'; yield 'f'; }
|> Seq.map(fun a -> a.ToString() )
|> Seq.reduce (fun a b -> a + b )


[ yield seq []; yield seq []; yield seq ["a"]; yield seq []; yield seq [] ] |> Seq.iter(fun a -> printfn "%A" a)

[ ["a";"b"];["a";"b"];["a";"b"] ] |> List.collect (fun a -> a) |> List.iter(fun a -> printfn "%A" a)


//|> List.iter(fun a -> printfn "%A" a)

sprintf "%s" "aaaaaa"




let source_words_2 =   [ "hello"; "world"; "goodbye"; "cruel"; "place" ]
                       |> Seq.filter (fun w -> w.Length > 1)
                       |> Seq.randomShuffle
                       |> Seq.toList







let letter_limit = 3
let word_limit = 5

//let testdata = ["11911911";"222999992222";"999993333333";"44444449999"]

//let words (word:string seq) = word |> Seq.mapi (fun count word -> (count,word) )

//let chars_in_words (x:(int*string) seq) = seq { for (count,word) in x do   yield seq { for letter in word.ToCharArray() do yield (count,word,letter) } }

//let replace_Word_Chars_with_Word_Data word_chars =

//                 seq {  for (count,word,c) in word_chars do
//                            if (int c - int '0') <= letter_limit then 
//                                yield! seq {for _ = 1 to (int c - int '0') do yield (count,word,'X')} 
//                            else
//                                yield! seq {for _ = 1 to letter_limit do yield (count,word,'L')} } 

//let limited_data_for_chars_in_items (a:seq<seq<int*string*char>>) = seq { for word_level in a do  yield replace_Word_Chars_with_Word_Data word_level }
                                                                                              
//let limited_data_for_the_word (a:(int*string*char) seq seq) = seq {for word in a do yield! (word |> Seq.truncate word_limit) }


//testdata
//|> words
//|> chars_in_words
//|> limited_data_for_chars_in_items
//|> limited_data_for_the_word  
//|> Seq.iter(fun out -> printfn "%A" out)


// better to expand out with additional fields in the records or additional record types.


let matches (char_level:(int*string*char)) =

             let (count,word,c) = char_level

             if c = 'o' then seq{ yield (count, word,'X'); yield (count, word,'X'); yield (count, word,'X'); yield (count, word,'X'); yield (count, word,'X') } else seq {yield char_level}

let word_list = [ "hohhhello"; "wowwwwwworld"; "gogggoodbye"; "coccccccccruel"; "poppppppplace" ]

let return_words_and_count (words:string seq)                      = words |> Seq.mapi (fun count word -> (count,word) )
let return_chars_for_word ((count,word):(int*string))              = seq { for letter in word.ToCharArray() do yield (count,word,letter) }
let return_limited_one_char_to_many_data (a:(int*string*char) seq) = a     |> Seq.map (fun c -> (matches c) |> Seq.truncate letter_limit ) |> Seq.collect (fun x -> x)
let return_limited_data_for_a_word (word:(int*string*char) seq)    = word  |> Seq.truncate word_limit


seq {    let word_count_seq = 
             word_list
             |> return_words_and_count

         for word in word_count_seq do // if you do not use the For then at least one of the functions will be a seq seq

             let limited_data =
                 word
                 |> return_chars_for_word
                 |> return_limited_one_char_to_many_data
                 |> return_limited_data_for_a_word

             yield! limited_data

}

|> Seq.iter (fun w -> printfn "%A" w)







type MyTuple = { Number: int; Text: string }

let mySequence: seq<MyTuple> = 
    seq {
        yield { Number = 1; Text = "One" }
        yield { Number = 2; Text = "Two" }
        yield { Number = 3; Text = "Three" }
    }

let printSequence (sequence: seq<MyTuple>) =
    sequence |> Seq.iter (fun tuple -> printfn "Number: %d, Text: %s" tuple.Number tuple.Text)


       
let xxx (aa:seq<MyTuple>) = seq { for { Number=num; Text=tx } in aa do yield '5' }


let identifyKeyChanges sequence =
    sequence
    |> Seq.pairwise
    |> Seq.filter (fun (prev, curr) -> fst prev <> fst curr)

let mySequence2 = seq { (1, "a"); (1, "b"); (2, "c"); (2, "d"); (3, "e") }

let keyChanges = identifyKeyChanges mySequence2

printfn "Key changes:"
keyChanges |> Seq.iter (fun (prev, curr) -> printfn "From %A to %A" prev curr)





let xy_word_selection_limit = 2

type Word_state3_data   = {data:string; flag1:string option}
type Word_state3_marker = { end_of_records_marker_for_a_word: string; word_count:int}
type Word_state3 =
| DATA3   of Word_state3_data
| MARKER3 of Word_state3_marker

type newOutput = {data:string; flag1:string}

let data = seq { DATA3({data="A"; flag1=None}); DATA3({data="B"; flag1=None}); DATA3({data="C"; flag1=Some"a"}); DATA3({data="D"; flag1=Some"b"}); DATA3({data="E"; flag1=Some"c"}); DATA3({data="F"; flag1=Some"d"}); DATA3({data="G"; flag1=None}); DATA3({data="H"; flag1=None}) }
let data2 = seq { DATA3({data="A"; flag1=None}); DATA3({data="B"; flag1=None}); DATA3({data="C"; flag1=None}); DATA3({data="D"; flag1=None}); DATA3({data="E"; flag1=None}); DATA3({data="F"; flag1=None}); DATA3({data="G"; flag1=None}); DATA3({data="H"; flag1=None}) }
let data3 = seq { MARKER3 ({end_of_records_marker_for_a_word="A"; word_count=10}); MARKER3 ({end_of_records_marker_for_a_word="B"; word_count=11}) }

let limit_matching_XY_per_word (data:Word_state3 seq) =

        data 

        |> Seq.scan (fun (data,count) x -> match x with
                                           | DATA3 a -> match a.flag1 with
                                                        | Some a -> match count with
                                                                    | count when count < xy_word_selection_limit -> ( Some x , (count + 1) )
                                                                    | _                                     -> ( None   , count       ) // no more candidate xy for a word
                                                        | None   -> ( Some x , count)
                                           | _       -> ( None , count ) )   (None , 0)

        |> Seq.map (fun (data,count) -> data)

        |> Seq.choose id


data
|> limit_matching_XY_per_word
|> Seq.iter (fun x -> printfn "%A" x)




let limit_matching_XY_per_word2 (data:Word_state3 seq) =

        let return_only_DATA3_data data =

            seq { for item in data do 
                   match item with
                   | DATA3 a -> yield a
                   | _       -> yield! Seq.empty
                }

        let return_only_Some_data (data:Word_state3_data seq) = 

            seq { for item in data do 
                   match item.flag1 with
                   | Some a -> yield {newOutput.data=item.data; flag1=a}     
                   | _      -> yield! Seq.empty
                }

        data
        |> return_only_DATA3_data
        |> return_only_Some_data
        |> Seq.truncate xy_word_selection_limit



data2
|> limit_matching_XY_per_word2
|> Seq.iter (fun x -> printfn "%A" x)


let aloop (data:Word_state3 seq) =

    seq {

        yield {newOutput.data="a"; flag1="first"}

        for d in data do

            yield! seq { {newOutput.data="a1"; flag1="b"};{newOutput.data="a2"; flag1="b"};{newOutput.data="a3"; flag1="b"} }

            yield {newOutput.data="a"; flag1="intermediate"}

        yield {newOutput.data="a"; flag1="last"}
            
        }


data2
|> aloop
|> Seq.iter (fun x -> printfn "%A" x)
















