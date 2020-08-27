namespace ResultUtils

module CustomResult =
    [<StructuralEquality; StructuralComparison>]
    [<CompiledName("FSharpResult`2")>]
    [<Struct>]
    type Result<'T,'TError> = 
      | Ok of ResultValue:'T 
      | Error of ErrorValue:'TError

[<RequireQualifiedAccess>]
module Result =

  let isOk x =
    match x with
    | CustomResult.Ok _ -> true
    | CustomResult.Error _ -> false

  let isError x =
    isOk x |> not

  let either okF errorF x =
    match x with
    | CustomResult.Ok x -> okF x
    | CustomResult.Error err -> errorF err

  let eitherMap okF errorF x =
    either (okF >> CustomResult.Ok) (errorF >> CustomResult.Error) x

  let bind binder result =
    match result with CustomResult.Error e -> CustomResult.Error e | CustomResult.Ok x -> binder x

  let map mapping result =
    match result with CustomResult.Error e -> CustomResult.Error e | CustomResult.Ok x -> CustomResult.Ok (mapping x)

  let mapError mapping result =
    match result with CustomResult.Error x -> CustomResult.Error (mapping x) | CustomResult.Ok v -> CustomResult.Ok v

  let apply f x =
    bind (fun f' ->
        bind (fun x' -> CustomResult.Ok (f' x')) x) f

  let map2 f x y =
    (apply (apply (CustomResult.Ok f) x) y)

  let map3 f x y z =
    apply (map2 f x y) z

  let fold onOk onError r =
    match r with
    | CustomResult.Ok x -> onOk x
    | CustomResult.Error y -> onError y
    
  let ofChoice c =
    match c with
    | Choice1Of2 x -> CustomResult.Ok x
    | Choice2Of2 x -> CustomResult.Error x

  let inline tryCreate fieldName (x : 'a) : CustomResult.Result< ^b, (string * 'c)> =
    let tryCreate' x =
      (^b : (static member TryCreate : 'a -> CustomResult.Result< ^b, 'c>) x)
    tryCreate' x
    |> mapError (fun z -> (fieldName, z))

  /// Replaces the wrapped value with unit
  let ignore result =
    result |> map ignore

  /// Returns the specified error if the value is false.
  let requireTrue error value =
    if value then CustomResult.Ok () else CustomResult.Error error

  /// Returns the specified error if the value is true.
  let requireFalse error value =
    if not value then CustomResult.Ok () else CustomResult.Error error

  /// Converts an Option to a Result, using the given error if None.
  let requireSome error option =
    match option with
    | Some x -> CustomResult.Ok x
    | None -> CustomResult.Error error

  /// Converts an Option to a Result, using the given error if Some.
  let requireNone error option =
    match option with
    | Some _ -> CustomResult.Error error
    | None -> CustomResult.Ok ()

  /// Converts a nullable value into a Result, using the given error if null
  let requireNotNull error value =
    match value with
    | null -> CustomResult.Error error
    | nonnull -> CustomResult.Ok nonnull

  /// Returns Ok if the two values are equal, or the specified error if not.
  /// Same as requireEqual, but with a signature that fits piping better than
  /// normal function application.
  let requireEqualTo other err this =
    if this = other then CustomResult.Ok () else CustomResult.Error err

  /// Returns Ok if the two values are equal, or the specified error if not.
  /// Same as requireEqualTo, but with a signature that fits normal function
  /// application better than piping.
  let requireEqual x1 x2 error =
    if x1 = x2 then CustomResult.Ok () else CustomResult.Error error

  /// Returns Ok if the sequence is empty, or the specified error if not.
  let requireEmpty error xs =
    if Seq.isEmpty xs then CustomResult.Ok () else CustomResult.Error error

  /// Returns the specified error if the sequence is empty, or Ok if not.
  let requireNotEmpty error xs =
    if Seq.isEmpty xs then CustomResult.Error error else CustomResult.Ok ()

  /// Returns the first item of the sequence if it exists, or the specified
  /// error if the sequence is empty
  let requireHead error xs =
    match Seq.tryHead xs with
    | Some x -> CustomResult.Ok x
    | None -> CustomResult.Error error

  /// Replaces an error value with a custom error value.
  let setError error result =
    result |> mapError (fun _ -> error)

  /// Replaces a unit error value with a custom error value. Safer than setError
  /// since you're not losing any information.
  let withError error result =
    result |> mapError (fun () -> error)

  /// Returns the contained value if Ok, otherwise returns ifError.
  let defaultValue ifError result =
    match result with
    | CustomResult.Ok x -> x
    | CustomResult.Error _ -> ifError

  /// Returns the contained value if Ok, otherwise evaluates ifErrorThunk and
  /// returns the result.
  let defaultWith ifErrorThunk result =
    match result with
    | CustomResult.Ok x -> x
    | CustomResult.Error _ -> ifErrorThunk ()

  let deref result =
    match result with
    | CustomResult.Ok x -> x
    | CustomResult.Error e -> failwithf "Failed to dereference Result (%A)" e
  /// Same as defaultValue for a result where the Ok value is unit. The name
  /// describes better what is actually happening in this case.
  let ignoreError result =
    defaultValue () result

  /// If the result is Ok and the predicate returns true, executes the function
  /// on the Ok value. Passes through the input value.
  let teeIf predicate f result =
    match result with
    | CustomResult.Ok x ->
        if predicate x then f x
    | CustomResult.Error _ -> ()
    result

  /// If the result is Error and the predicate returns true, executes the
  /// function on the Error value. Passes through the input value.
  let teeErrorIf predicate f result =
    match result with
    | CustomResult.Ok _ -> ()
    | CustomResult.Error x ->
        if predicate x then f x
    result

  /// If the result is Ok, executes the function on the Ok value. Passes through
  /// the input value.
  let tee f result =
    teeIf (fun _ -> true) f result

  /// If the result is Error, executes the function on the Error value. Passes
  /// through the input value.
  let teeError f result =
    teeErrorIf (fun _ -> true) f result

  let sequenceAsync (resAsync: CustomResult.Result<Async<'a>, 'b>) : Async<CustomResult.Result<'a, 'b>> =
    async {
      match resAsync with
      | CustomResult.Ok asnc ->
          let! x = asnc
          return CustomResult.Ok x
      | CustomResult.Error err -> return CustomResult.Error err
    }