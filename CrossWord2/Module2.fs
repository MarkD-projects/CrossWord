module Module2

open Module_Common



let isCellEmpty (coordinate:Coordinate) = not (coordinatesDict.ContainsKey coordinate)

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

    if isCellEmpty (coordinateAdjacentToStartLetter()) then 

       if isCellEmpty (coordinateAdjacentToEndLetter()) then

          let allCoordinates = allCoordinates() // cached

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


