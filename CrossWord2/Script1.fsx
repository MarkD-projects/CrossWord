




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




seq { for x in 1 .. 10 do yield x }
|> Seq.scan (fun state num -> match (num % 2) with
                              | 0 -> Seq.empty     // will output empty sequences
                              | _ -> seq {yield num}  ) (seq{yield 0})
|> Seq.iter(fun num -> printfn "%A" num)



seq {yield 'a'; yield 'b'; yield 'c'; yield 'd'; yield 'e'; yield 'f'; }
|> Seq.map(fun a -> a.ToString() )
|> Seq.reduce (fun a b -> a + b )


[ yield seq []; yield seq []; yield seq ["a"]; yield seq []; yield seq [] ] |> List.collect (fun a -> a) |> List.iter(fun a -> printfn "%A" a)

[ ["a";"b"];["a";"b"];["a";"b"] ] |> List.collect (fun a -> a) |> List.iter(fun a -> printfn "%A" a)


//|> List.iter(fun a -> printfn "%A" a)