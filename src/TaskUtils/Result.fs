namespace TaskUtils

open System.Threading.Tasks
open FSharp.Control.Tasks
open ResultUtils

[<RequireQualifiedAccess>]
module Result =
  let sequenceTask (resTask: Result<Task<'a>, 'b>) =

    task {
      match resTask with
      | Ok t ->
        let! x = t
        return Ok x
      | Error e -> return Error e
    }

[<RequireQualifiedAccess>]
module List =
  let private traverseTaskResultM' (f: 'c -> Task<CustomResult.Result<'a, 'b>>) (xs: 'c list) =
    let mutable state = CustomResult.Ok []
    let mutable index = 0
    let xs = xs |> List.toArray
    task {
        while state |> Result.isOk && index < xs.Length do
            let! r = xs |> Array.item index |> f
            index <- index + 1 
            match (r, state) with
            | CustomResult.Ok y, CustomResult.Ok ys ->
                state <- CustomResult.Ok (y :: ys)
            | CustomResult.Error e, _ ->
                state <- CustomResult.Error e
            | _, _ ->
                ()
        return 
            state
            |> Result.map List.rev
    }
  let traverseTaskResultM f xs =
    traverseTaskResultM' f xs

  let sequenceTaskResultM xs =
    traverseTaskResultM id xs

  let private traverseTaskResultA' (f : 'c -> Task<CustomResult.Result<'a,'b>>) (xs : 'c list) =
    let mutable state = CustomResult.Ok []
    
    task {
        for x in xs do
            let! r = f x 
            match (r, state) with
            | CustomResult.Ok y, CustomResult.Ok ys ->
                state <- CustomResult.Ok (y :: ys)
            | CustomResult.Error e, CustomResult.Error errs ->
                state <- CustomResult.Error (e :: errs)
            | CustomResult.Ok _, CustomResult.Error e ->
                state <- CustomResult.Error e
            | CustomResult.Error e , CustomResult.Ok _  -> 
                state <- CustomResult.Error [e]
        return
            state
            |> Result.eitherMap List.rev List.rev
    }

  let traverseTaskResultA f xs =
    traverseTaskResultA' f xs

  let sequenceTaskResultA xs =
    traverseTaskResultA id xs