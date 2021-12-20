module Day18

type SnailfishNumber(left: SnailfishNumber option, right: SnailfishNumber option, regularNumber: int option) =
    let mutable _left = left
    let mutable _right = right 
    let mutable _regularNumber = regularNumber 

    member this.Left with get() = _left and set(value) = _left <- value; _regularNumber <- None
    member this.Right with get() = _right and set(value) = _right <- value; _regularNumber <- None
    member this.RegularNumber with get() = _regularNumber and set(value) = _regularNumber <- value; _left <- None; _right <- None
        
    // override this.ToString() =
    //     if this.RegularNumber.IsSome then
    //         sprintf "value:%d" regularNumber.Value
    //     else ""
    member this.Height(): int =
        let leftHeight = if this.Left.IsSome then this.Left.Value.Height() + 1 else 0
        let rightHeight = if this.Right.IsSome then this.Right.Value.Height() + 1 else 0 

        seq { leftHeight; rightHeight } |> Seq.max

    new(input: char array) =
        let isPair = input.[0] = '['
        if isPair then
            let mutable stack = List.empty
            let mutable splitIndex = 0
            for i = 0 to input.Length - 1 do
                match input.[i] with
                | '[' -> stack <- input.[i] :: stack
                | ']' -> stack <- stack |> List.removeAt 0
                | ',' when stack.Length = 1 -> splitIndex <- i
                | _ -> ()
            let left = Some <| SnailfishNumber(input |> Array.skip 1 |> Array.take (splitIndex - 1))
            let right = Some <| SnailfishNumber(input |> Array.skip (splitIndex + 1) |> Array.take (input.Length - 2 - splitIndex))
            new SnailfishNumber(left, right, None)
        else if input.Length > 1 then
            let left = Some <| SnailfishNumber(input |> Array.take 1)
            let right = Some <| SnailfishNumber(input |> Array.skip 2 |> Array.take 1)
            new SnailfishNumber(left, right, None)
        else 
            new SnailfishNumber(None, None, Some <| (input.[0].ToString() |> System.Int32.Parse))

let rec leftMost (number: SnailfishNumber, height: int) =
    if number.Height() = height then
        (number, [])
    else 
        let foo = leftMost (number.Left.Value, height)
        (fst foo, snd foo @ [number])



let rec rightMost (number: SnailfishNumber) =
    if number.Height() = 1 then
        (number, [])
    else
        let foo = rightMost(number.Right.Value)
        (fst foo, snd foo @ [number])

let explode (number: SnailfishNumber) =
    let (pairToExplode, parents) = leftMost (number, 1) // traverse towards the node left most node with two leaves

    let leftSiblingParent = parents |> List.tryFind (fun x -> x.Left.IsSome)
    let rightSiblingParent = parents |> List.tryFind (fun x -> x.Right.IsSome)
    let foo = parents |> List.map (fun x -> x.Height())

    // if leftSiblingParent.IsSome then
    //     let (leftSibling, _) = rightMost (leftSiblingParent.Value.Left.Value)

    //     printfn "%A" leftSibling.RegularNumber.Value
    
    if rightSiblingParent.IsSome then
        printfn "%A" rightSiblingParent
        let rightTree = rightSiblingParent.Value.Right.Value
        if rightTree.RegularNumber.IsSome then
            printfn "%Arn" rightTree.RegularNumber.Value
            rightTree.RegularNumber <- Some <| (rightTree.RegularNumber.Value) + (pairToExplode.Right.Value.RegularNumber.Value)
            printfn "%Arn" rightTree.RegularNumber.Value
        else
            let (leftMost, _)= leftMost (rightTree, 0)
            leftMost.RegularNumber <- Some <| leftMost.RegularNumber.Value + pairToExplode.Right.Value.RegularNumber.Value
            printfn "%A" foo


    printfn "asdfasd %d %d %d" (number.Left.Value.Height()) (number.Right.Value.Height()) (pairToExplode.Height())
    pairToExplode.RegularNumber <- Some 0
    printfn "asdfasd %d %d %d" (number.Left.Value.Height()) (number.Right.Value.Height()) (pairToExplode.Height())
    number
        // if rightSiblingParent.Value.Right.Value.Left.IsNone then
        //     printfn "%Arn" rightSiblingParent.Value.RegularNumber.Value
        // else 
        //     let (rightSibling, _) = leftMost (rightSiblingParent.Value.Right.Value)

        //     printfn "%A" rightSibling.RegularNumber.Value

let addSnailfishNumbers (left: SnailfishNumber, right: SnailfishNumber) =
    let foo = new SnailfishNumber (Some left, Some right, None)

    printfn "%A" <| foo.Height()

    while foo.Height() > 4 do
        printfn "hi %d" <| foo.Height()
        if (foo.Left.Value.Height() >= 4) then
            foo.Left <- Some <| explode(left)
        else
            foo.Right <- Some <| explode(right)

    foo

type Day18() =
    inherit Base.Day()

    override this.partOne(input: seq<string>): int = 
        let foo = input |> Seq.map Seq.toArray |> Seq.map SnailfishNumber
        let bar = (foo |> Seq.reduce (fun a b -> addSnailfishNumbers(a, b)))

        explode bar
        // let baz = new SnailfishNumber([|'1'|])

        // bar.Left <- Some baz
        // bar.Right <- Some (new SnailfishNumber([|'2'|]))

        bar.Height()
    override this.partTwo(input: seq<string>): int = 
        failwith "Not Implemented"