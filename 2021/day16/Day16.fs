module Day16

open System.Collections

let convertCharToBinary (c: char) =

    let hex =
        System.Int32.Parse(c.ToString(), System.Globalization.NumberStyles.HexNumber)

    sprintf "%04B" hex |> Seq.toArray

printf "%A" (convertCharToBinary ('D'))

let convertBinaryToInt (binary: char []) =
    System.Convert.ToInt32(binary |> System.String, 2)

let convertBinaryToBig (binary: char []) =
    System.Convert.ToUInt64(binary |> System.String, 2)

let parseLiteralValue (transmission: char []) =

    let rec grabBits (transmission: char []) =
        let isLastGroup = transmission |> Array.head = '0'

        let bits =
            transmission |> Array.skip 1 |> Array.take 4

        if isLastGroup then
            bits
        else
            Array.concat [ bits
                           (grabBits (transmission |> Array.skip 5)) ]

    let bits = grabBits (transmission)

    (convertBinaryToBig bits, transmission |> Array.skip (5 * (bits.Length / 4)))

let operateValues (values: uint64 list, typeId: int) =
    match typeId with
    | 0 -> values |> List.sum
    | 1 -> values |> List.reduce (fun a b -> a * b)
    | 2 -> values |> List.min
    | 3 -> values |> List.max
    // TODO: assert that greater than or less than only has two
    // TODO: got my valeus backwards i think
    | 5 -> if values.[1] > values.[0] then uint64(1) else uint64(0)  
    | 6 -> if values.[1] < values.[0] then uint64(1) else uint64(0)  
    | 7 -> if values.[1] = values.[0] then uint64(1) else uint64(0)  

    | _ -> raise (System.ArgumentException())


type Day16() =
    inherit Base.Day()

    let mutable sumOfVersions = 0

    member this.parseOperator(transmission: char [], packetType: int) =
        let isTotalLengthType = transmission |> Array.head = '0'

        if isTotalLengthType then
            let length =
                transmission
                |> Array.skip 1
                |> Array.take 15
                |> convertBinaryToInt

            let mutable packets = transmission |> Array.skip (16)
            let mutable values = List.empty
            let mutable processed = 0

            while processed < length do
                let (value, (remaining: char [])) = this.parsePacket (packets)
                processed <- processed + packets.Length - remaining.Length
                packets <- remaining
                values <- value :: values

            (operateValues(values, packetType), packets)
        else
            let number =
                transmission
                |> Array.skip 1
                |> Array.take 11
                |> convertBinaryToInt


            let mutable packets = transmission |> Array.skip (12)
            let mutable values = List.empty

            for i = 0 to number - 1 do
                let (value, remaining) = this.parsePacket (packets)
                packets <- remaining
                values <- value :: values

            (operateValues(values, packetType), packets)


    member this.parsePacket(transmission: char []): (uint64 * char []) =
        let version =
            transmission |> Array.take 3 |> convertBinaryToInt

        sumOfVersions <- sumOfVersions + version

        let packetType =
            transmission
            |> Array.skip 3
            |> Array.take 3
            |> convertBinaryToInt

        if packetType = 4 then
            let (value, remaining) =
                parseLiteralValue (transmission |> Array.skip (3 + 3))

            (value, remaining)

        else
            let (value, remaining) =
                this.parseOperator (transmission |> Array.skip (3 + 3), packetType)

            (value, remaining)

    override this.partOne(input: seq<string>) : int =
        let transmission =
            input
            |> Seq.map Seq.toArray
            |> Seq.head
            |> Array.collect convertCharToBinary

        let (_, _) = (this.parsePacket (transmission))
        sumOfVersions


    override this.partTwo(input: seq<string>) : int =
        let transmission =
            input
            |> Seq.map Seq.toArray
            |> Seq.head
            |> Array.collect convertCharToBinary

        let (value, _) = (this.parsePacket (transmission))
        printfn "%u" value
        0
