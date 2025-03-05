

open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Diagnostics

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
















