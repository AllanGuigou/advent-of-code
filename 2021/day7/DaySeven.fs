module DaySeven

let sumFuel (crabs: int[], position: int) =
    crabs |> Seq.sumBy (fun (crab: int) -> abs (crab - position))

let summation (n: int) = ((n + 1) * n) / 2
let sumIncreasingFuel (crabs:int[], position) =
    crabs |> Seq.sumBy (fun (crab: int) -> summation <| abs (crab - position))


let rec binaryCrabSearch (crabs: int[], left, mid, right, sumFuelFn): int =
    let (leftSum, midSum, rightSum) = (sumFuelFn(crabs, left), sumFuelFn(crabs,mid), sumFuelFn(crabs, right))

    printf "L:%d M:%d R:%d\n" left mid right
    printf "LS:%d MS:%d RS:%d\n" leftSum midSum rightSum
    if left = mid || right = mid then
        printf "L:%d M:%d R:%d\n" left mid right
        printf "LS:%d MS:%d RS:%d\n" leftSum midSum rightSum
        [leftSum; midSum; rightSum] |> Seq.min
    else 
        if midSum + leftSum < rightSum + midSum then
            binaryCrabSearch(crabs, left, ((mid - left) / 2) + left, mid, sumFuelFn)
        else 
            binaryCrabSearch(crabs, mid, ((right - mid) / 2) + mid, right, sumFuelFn)

let parseCrabs (input: seq<string>)=
    (Seq.head input).Split [|','|]
    |> Array.map System.Int32.Parse
    |> Array.sort

type DaySeven() =
    inherit Base.Day()
    override this.partOne(input: seq<string>): int = 
        let crabs = parseCrabs input
        let left = Seq.min crabs
        let right = Seq.max crabs
        let mid = (right - left) / 2 

        binaryCrabSearch(crabs, left, mid, right, sumFuel)
    override this.partTwo(input: seq<string>): int = 
        let crabs = parseCrabs input
        let left = Seq.min crabs
        let right = Seq.max crabs
        let mid = (right - left) / 2 

        binaryCrabSearch(crabs, left, mid, right, sumIncreasingFuel)
