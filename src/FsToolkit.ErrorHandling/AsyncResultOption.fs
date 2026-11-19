namespace FsToolkit.ErrorHandling
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end

type AsyncResultOption<'ok, 'error> = Async<Result<'ok option, 'error>>

[<RequireQualifiedAccess>]
module AsyncResultOption =

    let inline map
        ([<InlineIfLambda>] mapper: 'okInput -> 'okOutput)
        (input: Async<Result<'okInput option, 'error>>)
        : Async<Result<'okOutput option, 'error>> =
        AsyncResult.map (Option.map mapper) input

    let inline bind
        ([<InlineIfLambda>] binder: 'okInput -> Async<Result<'okOutput option, 'error>>)
        (input: Async<Result<'okInput option, 'error>>)
        : Async<Result<'okOutput option, 'error>> =
        AsyncResult.bind
            (fun opt ->
                match opt with
                | Some x -> binder x
                | None -> AsyncResult.ok None
            )
            input

    let inline ok x =
        LogRiteLogging._logger.LogError(">>> Entering ok(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting ok(x={X})", x)
        Ok(Some x)
        |> Async.singleton

    let inline error x : Async<Result<'ok option, 'error>> =
        LogRiteLogging._logger.LogError(">>> Entering error(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting error(x={X})", x)
        Error x
        |> Async.singleton

    let inline map2
        ([<InlineIfLambda>] mapper: 'okInput1 -> 'okInput2 -> 'okOutput)
        (input1: Async<Result<'okInput1 option, 'error>>)
        (input2: Async<Result<'okInput2 option, 'error>>)
        : Async<Result<'okOutput option, 'error>> =
        AsyncResult.map2 (Option.map2 mapper) input1 input2

    let inline map3
        ([<InlineIfLambda>] mapper: 'okInput1 -> 'okInput2 -> 'okInput3 -> 'okOutput)
        (input1: Async<Result<'okInput1 option, 'error>>)
        (input2: Async<Result<'okInput2 option, 'error>>)
        (input3: Async<Result<'okInput3 option, 'error>>)
        : Async<Result<'okOutput option, 'error>> =
        AsyncResult.map3 (Option.map3 mapper) input1 input2 input3

    let inline singleton (value: 'ok) : Async<Result<'ok option, 'error>> =
        AsyncResult.ok (Some value)

    let apply
        (applier: Async<Result<('okInput -> 'okOutput) option, 'error>>)
        (input: Async<Result<'okInput option, 'error>>)
        : Async<Result<'okOutput option, 'error>> =
        map2 (fun f x -> f x) applier input

#if !FABLE_COMPILER_PYTHON
    // https://github.com/fable-compiler/Fable/issues/4125
    /// Replaces the wrapped value with unit
    let ignore<'ok, 'error>
        (value: Async<Result<'ok option, 'error>>)
        : Async<Result<unit option, 'error>> =
        value
        |> map ignore<'ok>
#endif


    let inline ofResult (r: Result<'ok, 'error>) =
        r
        |> Result.map Some
        |> Async.singleton


    let inline ofAsyncResult (r: Async<Result<'ok, 'error>>) =
        LogRiteLogging._logger.LogError(">>> Entering ofAsyncResult(r={R})", r)
        r
        |> AsyncResult.map Some

    let inline ofOption (r: 'ok option) =
        LogRiteLogging._logger.LogError(">>> Entering ofOption(r={R})", r)
        r
        |> Ok
        |> Async.singleton

    let inline ofAsyncOption (r: Async<'ok option>) =
        LogRiteLogging._logger.LogError(">>> Entering ofAsyncOption(r={R})", r)
        r
        |> Async.map Ok
