namespace TaskUtils

open FSharp.Control.Tasks
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module TaskResult =
  open ResultUtils
  let map f tr =
    Task.map(ResultExtensions.map f) tr

  let bind f (tr: Task<_>) = task {
    let! result = tr
    let t =
      match result with
      | Ok x -> f x
      | Error e -> task { return Error e }
    return! t
  }


  let foldResult onSuccess onError tr =
    Task.map (ResultExtensions.fold onSuccess onError) tr

  let ofAsync aAsync = 
    aAsync
    |> Async.Catch 
    |> Async.StartAsTask 
    |> Task.map ResultExtensions.ofChoice
  
  let retn x =
    Ok x
    |> Task.singleton
  
  let returnError x =
    Error x
    |> Task.singleton

  let map2 f xTR yTR =
    Task.map2 (ResultExtensions.map2 f) xTR yTR

  let map3 f xTR yTR zTR =
    Task.map3 (ResultExtensions.map3 f) xTR yTR zTR

  let apply fTR xTR =
    map2 (fun f x -> f x) fTR xTR

  /// Replaces the wrapped value with unit
  let ignore tr =
      tr |> map ignore

  /// Returns the specified error if the task-wrapped value is false.
  let requireTrue error value = 
    value |> Task.map (ResultExtensions.requireTrue error)

  /// Returns the specified error if the task-wrapped value is true.
  let requireFalse error value =
    value |> Task.map (ResultExtensions.requireFalse error) 

  // Converts an task-wrapped Option to a Result, using the given error if None.
  let requireSome error option =
    option |> Task.map (ResultExtensions.requireSome error)

  // Converts an task-wrapped Option to a Result, using the given error if Some.
  let requireNone error option =
    option |> Task.map (ResultExtensions.requireNone error)

  /// Returns Ok if the task-wrapped value and the provided value are equal, or the specified error if not.
  let requireEqual x1 x2 error =
    x2 |> Task.map (fun x2' -> ResultExtensions.requireEqual x1 x2' error)

  /// Returns Ok if the two values are equal, or the specified error if not.
  let requireEqualTo other error this =
    this |> Task.map (ResultExtensions.requireEqualTo other error)

  /// Returns Ok if the task-wrapped sequence is empty, or the specified error if not.
  let requireEmpty error xs =
    xs |> Task.map (ResultExtensions.requireEmpty error)

  /// Returns Ok if the task-wrapped sequence is not-empty, or the specified error if not.
  let requireNotEmpty error xs =
    xs |> Task.map (ResultExtensions.requireNotEmpty error)

  /// Returns the first item of the task-wrapped sequence if it exists, or the specified
  /// error if the sequence is empty
  let requireHead error xs =
    xs |> Task.map (ResultExtensions.requireHead error)

  /// Replaces an error value of an task-wrapped result with a custom error
  /// value.
  let setError error taskResult =
    taskResult |> Task.map (ResultExtensions.setError error)

  /// Replaces a unit error value of an task-wrapped result with a custom
  /// error value. Safer than setError since you're not losing any information.
  let withError error taskResult =
    taskResult |> Task.map (ResultExtensions.withError error)

  /// Extracts the contained value of an task-wrapped result if Ok, otherwise
  /// uses ifError.
  let defaultValue ifError taskResult =
    taskResult |> Task.map (ResultExtensions.defaultValue ifError)

  /// Extracts the contained value of an task-wrapped result if Ok, otherwise
  /// evaluates ifErrorThunk and uses the result.
  let defaultWith ifErrorThunk taskResult =
    taskResult |> Task.map (ResultExtensions.defaultWith ifErrorThunk)

  /// Same as defaultValue for a result where the Ok value is unit. The name
  /// describes better what is actually happening in this case.
  let ignoreError taskResult =
    defaultValue () taskResult

  /// If the task-wrapped result is Ok, executes the function on the Ok value.
  /// Passes through the input value.
  let tee f taskResult =
    taskResult |> Task.map (ResultExtensions.tee f)

  /// If the task-wrapped result is Ok and the predicate returns true, executes
  /// the function on the Ok value. Passes through the input value.
  let teeIf predicate f taskResult =
    taskResult |> Task.map (ResultExtensions.teeIf predicate f)

  /// If the task-wrapped result is Error, executes the function on the Error
  /// value. Passes through the input value.
  let teeError f taskResult =
    taskResult |> Task.map (ResultExtensions.teeError f)

  /// If the task-wrapped result is Error and the predicate returns true,
  /// executes the function on the Error value. Passes through the input value.
  let teeErrorIf predicate f taskResult =
    taskResult |> Task.map (ResultExtensions.teeErrorIf predicate f)
