namespace FsToolkit.ErrorHandling


open System.Threading.Tasks
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module Task =
    let inline singleton value =
        LogRiteLogging._logger.LogError(">>> Entering singleton(value={Value})", value)
        LogRiteLogging._logger.LogWarning("<<< Exiting singleton(value={Value})", value)
        value
        |> Task.FromResult

    let inline bind ([<InlineIfLambda>] f: 'a -> Task<'b>) (x: Task<'a>) =
        LogRiteLogging._logger.LogError(">>> Entering bind()")
        LogRiteLogging._logger.LogWarning("<<< Exiting bind()")
        task {
            let! x = x
            return! f x
        }


    let inline bindV ([<InlineIfLambda>] f: 'a -> Task<'b>) (x: ValueTask<'a>) =
        LogRiteLogging._logger.LogError(">>> Entering bindV()")
        LogRiteLogging._logger.LogWarning("<<< Exiting bindV()")
        task {
            let! x = x
            return! f x
        }

    let inline apply f x =
        LogRiteLogging._logger.LogError(">>> Entering apply(f={F},x={X})", f, x)
        LogRiteLogging._logger.LogWarning("<<< Exiting apply(f={F},x={X})", f, x)
        task {
            let! f' = f
            let! x' = x
            return f' x'
        }

    let inline map ([<InlineIfLambda>] f) x =
        LogRiteLogging._logger.LogError(">>> Entering map()")
        LogRiteLogging._logger.LogWarning("<<< Exiting map()")
        x
        |> bind (
            f
            >> singleton
        )

    let inline mapV ([<InlineIfLambda>] f) x =
        LogRiteLogging._logger.LogError(">>> Entering mapV()")
        LogRiteLogging._logger.LogWarning("<<< Exiting mapV()")
        x
        |> bindV (
            f
            >> singleton
        )

    let inline map2 ([<InlineIfLambda>] f) x y =
        LogRiteLogging._logger.LogError(">>> Entering map2()")
        LogRiteLogging._logger.LogWarning("<<< Exiting map2()")
        task {
            let! x' = x
            let! y' = y
            return f x' y'
        }

    let inline map3 ([<InlineIfLambda>] f) x y z =
        LogRiteLogging._logger.LogError(">>> Entering map3()")
        LogRiteLogging._logger.LogWarning("<<< Exiting map3()")
        task {
            let! x' = x
            let! y' = y
            let! z' = z
            return f x' y' z'
        }

    /// Allows us to call `do!` syntax inside a computation expression
    let inline ignore<'a> (x: Task<'a>) =
        LogRiteLogging._logger.LogError(">>> Entering ignore(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting ignore(x={X})", x)
        x
        |> map ignore


    /// Takes two tasks and returns a tuple of the pair
    let zip (a1: Task<_>) (a2: Task<_>) =
        LogRiteLogging._logger.LogError(">>> Entering zip(a1={A1})", a1)
        LogRiteLogging._logger.LogWarning("<<< Exiting zip(a1={A1})", a1)
        task {
            let! r1 = a1
            let! r2 = a2
            return r1, r2
        }

    let ofUnit (t: Task) = task { return! t }

    /// Creates a `Task` that attempts to execute the provided task,
    /// returning `Choice1Of2` with the result if the task completes
    /// without exceptions, or `Choice2Of2` with the exception if an
    /// exception is thrown.
    let catch (x: Task<_>) =
        LogRiteLogging._logger.LogError(">>> Entering catch(x={X})", x)
        task {
            try
                LogRiteLogging._logger.LogError("Exception in catch(x={X}): {Arg1}", x, ex.Message)
                let! r = x
                return Choice1Of2 r
            with e ->
                LogRiteLogging._logger.LogError("Exception in catch(x={X}): {Arg1}", x, e.Message)
                LogRiteLogging._logger.LogWarning("<<< Exiting catch(x={X})", x)
                return Choice2Of2 e
        }
