namespace ResultUtils

[<AutoOpen>]
module ResultOptionOp =

  let inline (<!>) f x = ResultOption.map f x
  let inline (<*>) f x = ResultOption.apply f x
  let inline (<*^>) f x = ResultOption.apply f (ResultExtensions.map Some x)
  let inline (>>=) x f = ResultOption.bind f x