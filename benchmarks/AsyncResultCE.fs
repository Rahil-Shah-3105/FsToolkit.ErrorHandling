namespace FsToolkit.ErrorHandling.Benchmarks

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open FsToolkit.ErrorHandling
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
module AsyncResultCE =


    type AsyncResultInlinedLambdaBuilder() =

        member _.Return(value: 'T) : Async<Result<'T, 'TError>> =
            async.Return
            <| result.Return value

        member inline _.ReturnFrom
            (asyncResult: Async<Result<'T, 'TError>>)
            : Async<Result<'T, 'TError>> =
            asyncResult

        member _.Zero() : Async<Result<unit, 'TError>> =
            LogRiteLogging._logger.LogError(">>> Entering Zero()")
            async.Return
            <| result.Zero()

        member inline _.Bind
            (
                asyncResult: Async<Result<'T, 'TError>>,
                [<InlineIfLambda>] binder: 'T -> Async<Result<'U, 'TError>>
            ) : Async<Result<'U, 'TError>> =
            async.Bind(
                asyncResult,
                fun r ->
                    match r with
                    | Ok x -> binder x
                    | Error e ->
                        Error e
                        |> async.Return
            )

        member inline _.Delay
            ([<InlineIfLambda>] generator: unit -> Async<Result<'T, 'TError>>)
            : Async<Result<'T, 'TError>> =
            async.Delay generator

        /// <summary>
        /// Method lets us transform data types into our internal representation.  This is the identity method to recognize the self type.
        ///
        /// See https://stackoverflow.com/questions/35286541/why-would-you-use-builder-source-in-a-custom-computation-expression-builder
        /// </summary>
        member inline _.Source(result: Async<Result<_, _>>) : Async<Result<_, _>> = result

open AsyncResultCE


[<AutoOpen>]
// Having members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
module AsyncResultCEExtensions =

    type AsyncResultInlinedLambdaBuilder with

        /// <summary>
        /// Needed to allow `for..in` and `for..do` functionality
        /// </summary>
        member inline _.Source(s: #seq<_>) = s

        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        member inline _.Source(result: Result<_, _>) : Async<Result<_, _>> = Async.singleton result

    let rec fib n =
        LogRiteLogging._logger.LogError(">>> Entering fib(n={N})", n)
        if n < 2L then
            LogRiteLogging._logger.LogWarning("<<< Exiting fib(n={N})", n)
            n
        else
            fib (n - 1L)
            + fib (n - 2L)

    let rec afib (n, level) =
        LogRiteLogging._logger.LogError(">>> Entering afib(n={N},level={Level})", n, level)
        async {
            if n < 0L then
                LogRiteLogging._logger.LogWarning("<<< Exiting afib(n={N},level={Level})", n, level)
                return Error "No"
            elif n < 2L then
                return Ok n
            elif n < level then
                return Ok(fib n)
            else
                let! n2a =
                    afib (n - 2L, level)
                    |> Async.StartChild

                let! n1 = afib (n - 1L, level)
                let! n2 = n2a

                match n1, n2 with
                | Ok n1, Ok n2 -> return Ok(n2 + n1)
                | Error e, _
                | _, Error e -> return Error e
        }

    let asyncResultInlinedIfLambda = AsyncResultInlinedLambdaBuilder()

    [<MemoryDiagnoser>]
    type AsyncResult_BindCEBenchmarks() =

        [<Benchmark(Baseline = true)>]
        member this.afib() =
            afib (10, 5)
            |> Async.StartImmediateAsTask

        [<Benchmark>]
        member this.Result_Normal_Bind_CE() =
            LogRiteLogging._logger.LogError(">>> Entering Result_Normal_Bind_CE()")
            let action () =
                asyncResult {
                    let! a = Ok 10
                    let! b = Ok 5
                    let! c = afib (a, b)
                    return c
                }

            action ()
            |> Async.StartImmediateAsTask


        [<Benchmark>]
        member this.Result_Alt_Inlined_Bind_CE() =
            LogRiteLogging._logger.LogError(">>> Entering Result_Alt_Inlined_Bind_CE()")
            let action () =
                LogRiteLogging._logger.LogError(">>> Entering action()")
                asyncResultInlinedIfLambda {
                    let! a = Ok 10
                    let! b = Ok 5
                    let! c = afib (a, b)
                    return c
                }

            action ()
            |> Async.StartImmediateAsTask
