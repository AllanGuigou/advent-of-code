module Day16

open System.Collections

let convertCharToBinary (c: char) =

    let hex =
        System.Int32.Parse(c.ToString(), System.Globalization.NumberStyles.HexNumber)

    sprintf "%04B" hex |> Seq.toArray

printf "%A" (convertCharToBinary ('D'))

let convertBinaryToInt (binary: char []) =
    System.Convert.ToInt32(binary |> System.String, 2)

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

    (0, transmission |> Array.skip (5 * (bits.Length / 4)))




type Day16() =
    inherit Base.Day()

    let mutable sumOfVersions = 0

    member this.parseOperator(transmission: char []) =
        let isTotalLengthType = transmission |> Array.head = '0'

        if isTotalLengthType then
            let length =
                transmission
                |> Array.skip 1
                |> Array.take 15
                |> convertBinaryToInt

            let mutable packets = transmission |> Array.skip (16)
            let mutable packetsValue = 0
            let mutable processed = 0

            while processed < length do
                let (value, (remaining: char [])) = this.parsePacket (packets)
                processed <- processed + packets.Length - remaining.Length
                packets <- remaining
                packetsValue <- packetsValue + value

            (packetsValue, packets)
        else
            let number =
                transmission
                |> Array.skip 1
                |> Array.take 11
                |> convertBinaryToInt


            let mutable packets = transmission |> Array.skip (12)
            let mutable packetsValue = 0

            for i = 0 to number - 1 do
                let (value, remaining) = this.parsePacket (packets)
                packets <- remaining
                packetsValue <- packetsValue + value

            (packetsValue, packets)


    member this.parsePacket(transmission: char []) =
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
                this.parseOperator (transmission |> Array.skip (3 + 3))

            (value, remaining)

    override this.partOne(input: seq<string>) : int =
        let transmission =
            input
            |> Seq.map Seq.toArray
            |> Seq.head
            |> Array.collect convertCharToBinary

        let (_, _) = (this.parsePacket (transmission))
        sumOfVersions


    override this.partTwo(input: seq<string>) : int = failwith "Not Implemented"
