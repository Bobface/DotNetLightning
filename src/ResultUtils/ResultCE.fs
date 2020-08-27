namespace ResultUtils

open System

[<AutoOpen>]
module ResultCE =
  type ResultBuilder() =
    member __.Return (v: 'T) : CustomResult.Result<'T, 'TError> =
      CustomResult.Ok v

    member __.ReturnFrom (result: Result<'T, 'TError>) : Result<'T, 'TError> =
      result

    member this.Zero () : CustomResult.Result<unit, 'TError> =
      this.Return ()

    member __.Bind
        (result: CustomResult.Result<'T, 'TError>, binder: 'T -> CustomResult.Result<'U, 'TError>)
        : CustomResult.Result<'U, 'TError> =
      Result.bind binder result

    member __.Delay
        (generator: unit -> CustomResult.Result<'T, 'TError>)
        : unit -> CustomResult.Result<'T, 'TError> =
      generator

    member __.Run
        (generator: unit -> CustomResult.Result<'T, 'TError>)
        : CustomResult.Result<'T, 'TError> =
      generator ()

    member this.Combine
        (result: CustomResult.Result<unit, 'TError>, binder: unit -> CustomResult.Result<'T, 'TError>)
        : CustomResult.Result<'T, 'TError> =
      this.Bind(result, binder)

    member this.TryWith
        (generator: unit -> CustomResult.Result<'T, 'TError>,
         handler: exn -> CustomResult.Result<'T, 'TError>)
        : CustomResult.Result<'T, 'TError> =
      try this.Run generator with | e -> handler e

    member this.TryFinally
        (generator: unit -> CustomResult.Result<'T, 'TError>, compensation: unit -> unit)
        : CustomResult.Result<'T, 'TError> =
      try this.Run generator finally compensation ()

    member this.Using
        (resource: 'T when 'T :> IDisposable, binder: 'T -> CustomResult.Result<'U, 'TError>)
        : CustomResult.Result<'U, 'TError> =
      this.TryFinally (
        (fun () -> binder resource),
        (fun () -> if not <| obj.ReferenceEquals(resource, null) then resource.Dispose ())
      )

    member this.While
        (guard: unit -> bool, generator: unit -> CustomResult.Result<unit, 'TError>)
        : CustomResult.Result<unit, 'TError> =
      if not <| guard () then this.Zero ()
      else this.Bind(this.Run generator, fun () -> this.While (guard, generator))

    member this.For
        (sequence: #seq<'T>, binder: 'T -> CustomResult.Result<unit, 'TError>)
        : CustomResult.Result<unit, 'TError> =
      this.Using(sequence.GetEnumerator (), fun enum ->
        this.While(enum.MoveNext,
          this.Delay(fun () -> binder enum.Current)))

[<AutoOpen>]
module ResultCEExtensions =

  // Having Choice<_> members as extensions gives them lower priority in
  // overload resolution and allows skipping more type annotations.
  type ResultBuilder with

    member __.ReturnFrom (result: Choice<'T, 'TError>) : CustomResult.Result<'T, 'TError> =
      Result.ofChoice result

    member __.Bind
        (result: Choice<'T, 'TError>, binder: 'T -> CustomResult.Result<'U, 'TError>)
        : CustomResult.Result<'U, 'TError> =
        result
        |> Result.ofChoice
        |> Result.bind binder 



  let result = ResultBuilder()