module Base

[<AbstractClass>]
type Day() =
    abstract member partOne : seq<string> -> int // TODO: does this need to be larger than an int?
    abstract member partTwo : seq<string> -> int