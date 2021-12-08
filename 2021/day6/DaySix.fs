module DaySix

type Lanternfish =
    struct
        val mutable gestationTimer: int

        member this.progressGestationTimer =
            let mutable result = []

            if this.gestationTimer > 0 then
                this.gestationTimer <- this.gestationTimer - 1
                result <- [ this ]
            else
                this.gestationTimer <- 6
                result <- [ this; Lanternfish(8) ]

            result

        override this.ToString() = sprintf "%d" this.gestationTimer

        new(initialGestationTimer: string) = { gestationTimer = System.Int32.Parse initialGestationTimer }
        new(initialGestationTimer: int) = { gestationTimer = initialGestationTimer }
    end

let parseInput (input: seq<string>) =
    input
    |> Seq.toList
    |> List.head
    |> fun x -> x.Split ','
    |> Array.map Lanternfish
    |> Array.toList

let progressLanternFishGestationTimers (lanternfish: list<Lanternfish>) =
    lanternfish
    |> List.collect (fun x -> x.progressGestationTimer)

type DaySix() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let mutable lanternfish = parseInput input

        let days = 80

        for i = 0 to days - 1 do
            // TODO: is it possible to mutate the items in the list w/o replacing the list?
            lanternfish <- (progressLanternFishGestationTimers lanternfish)
            printf "%A\n" lanternfish


        lanternfish.Length

    override this.partTwo(input: seq<string>) : int =
        let lanternfish =
            parseInput input
            |> List.map (fun (x: Lanternfish) -> x.gestationTimer)

        let aggregateLanternfish =
            Array.zeroCreate 9
            |> Array.map (fun x -> uint64 (x))

        for x in lanternfish do
            aggregateLanternfish.[x] <- aggregateLanternfish.[x] + uint64 (1)


        let days = 256

        printf "%A" aggregateLanternfish

        for i = 0 to days - 1 do
            let internalTimerComplete = aggregateLanternfish.[0]

            for i = 0 to 7 do
                aggregateLanternfish.[i] <- aggregateLanternfish.[i + 1]

            aggregateLanternfish.[6] <- internalTimerComplete + aggregateLanternfish.[6]
            aggregateLanternfish.[8] <- internalTimerComplete


        printf "%A\n" aggregateLanternfish

        let bigNumber =
            aggregateLanternfish
            |> Array.reduce (fun x y -> x + y)

        printf "%u\n" bigNumber
        0
