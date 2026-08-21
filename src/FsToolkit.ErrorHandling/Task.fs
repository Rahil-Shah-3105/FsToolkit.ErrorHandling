namespace FsToolkit.ErrorHandling


open System.Threading.Tasks
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module Task =
    let inline singleton value =
        LogRiteLogging._logger.LogDebug(">>> Entering inline(singleton={Singleton},value={Value})", singleton, value)
        value
        LogRiteLogging._logger.LogDebug("<<< Exiting inline(bind={Bind},x={X})", bind, x)
        |> Task.FromResult

    let inline bind ([<InlineIfLambda>] f: 'a -> Task<'b>) (x: Task<'a>) =
        task {
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(bind={Bind},x={X})", bind, x)
            let! x = x
            LogRiteLogging._logger.LogDebug(">>> Entering inline(bindV={BindV},x={X})", bindV, x)
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(bindV={BindV},x={X})", bindV, x)
            return! f x
        }


    let inline bindV ([<InlineIfLambda>] f: 'a -> Task<'b>) (x: ValueTask<'a>) =
        task {
            LogRiteLogging._logger.LogDebug(">>> Entering inline(bindV={BindV},x={X})", bindV, x)
            let! x = x
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(bindV={BindV},x={X})", bindV, x)
            LogRiteLogging._logger.LogDebug(">>> Entering inline(apply={Apply},f={F},x={X})", apply, f, x)
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(apply={Apply},f={F},x={X})", apply, f, x)
            return! f x
        }

    let inline apply f x =
        task {
            let! f' = f
            let! x' = x
            return f' x'
        }

    let inline map ([<InlineIfLambda>] f) x =
        x
        |> bind (
            f
            >> singleton
        )

    let inline mapV ([<InlineIfLambda>] f) x =
        x
        |> bindV (
            f
        LogRiteLogging._logger.LogDebug(">>> Entering inline(map2={Map2})", map2)
        LogRiteLogging._logger.LogDebug("<<< Exiting inline(map2={Map2})", map2)
            >> singleton
        )

    let inline map2 ([<InlineIfLambda>] f) x y =
        task {
            let! x' = x
            let! y' = y
            LogRiteLogging._logger.LogDebug(">>> Entering inline(map3={Map3})", map3)
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(map3={Map3})", map3)
            return f x' y'
        }

    let inline map3 ([<InlineIfLambda>] f) x y z =
        task {
            let! x' = x
            let! y' = y
            let! z' = z
            return f x' y' z'
        }

    /// Allows us to call `do!` syntax inside a computation expression
    let inline ignore<'a> (x: Task<'a>) =
        x
        |> map ignore


    /// Takes two tasks and returns a tuple of the pair
    let zip (a1: Task<_>) (a2: Task<_>) =
        task {
        LogRiteLogging._logger.LogDebug(">>> Entering zip(a1={A1})", a1)
            let! r1 = a1
            LogRiteLogging._logger.LogDebug("<<< Exiting zip(a1={A1})", a1)
            let! r2 = a2
            return r1, r2
        }

    let ofUnit (t: Task) = task { return! t }

    /// Creates a `Task` that attempts to execute the provided task,
    /// returning `Choice1Of2` with the result if the task completes
    /// without exceptions, or `Choice2Of2` with the exception if an
    /// exception is thrown.
    let catch (x: Task<_>) =
        task {
        LogRiteLogging._logger.LogDebug(">>> Entering catch(x={X})", x)
            try
                LogRiteLogging._logger.LogDebug("<<< Exiting catch(x={X})", x)
                let! r = x
                LogRiteLogging._logger.LogError("Exception in catch(x={X}): {Arg1}", x, ex.Message)
                return Choice1Of2 r
            with e ->
                LogRiteLogging._logger.LogError("Exception in catch(x={X}): {Arg1}", x, e.Message)
                return Choice2Of2 e
        }
