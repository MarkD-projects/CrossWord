




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
//|> top_level_read
|> second_level
|> Seq.iter(fun c -> printfn "iter %A" c)



