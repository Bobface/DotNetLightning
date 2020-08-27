namespace ResultUtils

[<RequireQualifiedAccess>]
module Validation =
  let ofResult x =
    Result.mapError List.singleton x

  let apply f x =
    match f, x with
    | CustomResult.Ok f, CustomResult.Ok x -> CustomResult.Ok (f x)
    | CustomResult.Error errs, CustomResult.Ok _ | CustomResult.Ok _, CustomResult.Error errs -> CustomResult.Error errs
    | CustomResult.Error errors1, CustomResult.Error errors2 -> CustomResult.Error (errors1 @ errors2)

  let retn x = ofResult (CustomResult.Ok x)

  let map2 f x y =
    apply (apply (retn f ) x ) y

  let map3 f x y z =
    apply (map2 f x y) z
    
  let inline lift2 (f: 'a -> 'b -> 'c) (x: CustomResult.Result<'a, _>) (y: CustomResult.Result<'b, _>): CustomResult.Result<'c, _> =
    apply (apply (CustomResult.Ok f) x) y

