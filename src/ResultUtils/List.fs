namespace ResultUtils

[<RequireQualifiedAccess>]
module List =
    let rec private traverseResultM' (state: CustomResult.Result<_, _>) (f: _ -> CustomResult.Result<_, _>) xs =
        match xs with
        | [] ->
            state
        | x :: xs ->
            let r = result {
                let! y = f x
                let! ys = state
                return ys @ [y]
            }
            match r with
            | CustomResult.Ok _ -> traverseResultM' r f xs
            | CustomResult.Error _ -> r
            
    let traverseResultM f xs =
        traverseResultM' (CustomResult.Ok []) f xs
        
    let sequenceResultM xs =
        traverseResultM id xs

    let rec private traverseResultA' state f xs =
        match xs with
        | [] -> state
        | x :: xs ->
            let fR =
                f x |> Result.mapError List.singleton
            match state, fR with
            | CustomResult.Ok ys, CustomResult.Ok y ->
                traverseResultA' (CustomResult.Ok (ys @ [y])) f xs
            | CustomResult.Error errs, CustomResult.Error e ->
                traverseResultA' (CustomResult.Error(errs @ e)) f xs
            | CustomResult.Ok _, CustomResult.Error e | CustomResult.Error e , CustomResult.Ok _ ->
                traverseResultA' (CustomResult.Error e) f xs

    let traverseResultA f xs =
        traverseResultA' (CustomResult.Ok []) f xs
    let sequenceResultA xs =
        traverseResultA id xs