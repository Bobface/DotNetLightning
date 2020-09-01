namespace ResultUtils

[<AutoOpen>]
module ResultOp =
  let inline (<!>) f x = ResultExtensions.map f x
  let inline (<*>) f x = ResultExtensions.apply f x
  let inline (>>=) x f = ResultExtensions.bind f x
