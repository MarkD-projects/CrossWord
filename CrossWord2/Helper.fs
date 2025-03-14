module Helper

open System.IO

open Module_Common

let writer2 = new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, "debug_info.txt"))

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

    seq {
            for y in y_top .. -1 .. y_bottom do

                let row_data = seq { for x in x_left .. x_right do yield (helperprintout x y) }

                let return_a_row = row_data 
                                   |> Seq.map(fun a -> a.ToString() )
                                   |> Seq.reduce (fun a b -> a + b)

                yield sprintf "%s" return_a_row
        }

let printBlock2 x y =

    let y_top     = y + 12
    let y_bottom  = y - 12
    let x_right   = x + 12
    let x_left    = x - 12

    seq {
            for y in y_top .. -1 .. y_bottom do

                let row_data = seq { for x in x_left .. x_right do yield (helperprintout x y) }

                let return_a_row = row_data 
                                   |> Seq.map(fun a -> a.ToString() )
                                   |> Seq.reduce (fun a b -> a + b)

                yield sprintf "%s" return_a_row
        }


let debug b =

   seq { for a in b do
             printfn "debug %A" a
             yield a
       }

let writeGrid_to_file() =

#if DEBUG
    use writer  = new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, "the_grid.txt"))
#else
    use writer = new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, "the_grid_txt"))
#endif

    printBlock() |> Seq.iter(fun line -> writer.WriteLine(line) )

//printBlock()

//for kvp in letters         do printfn "Key: %A, Value: %A" kvp.Key kvp.Value.Length
//for kvp in coordinatesDict do printfn "Key: %A, Value: %A" kvp.Key kvp.Value


