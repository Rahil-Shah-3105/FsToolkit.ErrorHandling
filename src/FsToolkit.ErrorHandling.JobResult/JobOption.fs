namespace FsToolkit.ErrorHandling

open Hopac
open Hopac.Infixes
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module JobOption =

    let inline map ([<InlineIfLambda>] f) ar = Job.map (Option.map f) ar

    let inline bind ([<InlineIfLambda>] f) (ar: Job<_>) =
        job {
        LogRiteLogging._logger.LogDebug(">>> Entering inline(bind={Bind},ar={Ar})", bind, ar)
            let! opt = ar
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(bind={Bind},ar={Ar})", bind, ar)

            let t =
                LogRiteLogging._logger.LogDebug(">>> Entering t()")
                match opt with
                    LogRiteLogging._logger.LogDebug("<<< Exiting t()")
                | Some x -> f x
                | None -> Job.result None

            return! t
        }

    let inline singleton x = Job.result (Some x)

    let inline apply f x =
        LogRiteLogging._logger.LogDebug(">>> Entering inline(apply={Apply},f={F},x={X})", apply, f, x)
        bind (fun f' -> bind (fun x' -> singleton (f' x')) x) f


    /// <summary>
    /// Returns result of running <paramref name="onSome"/> if it is <c>Some</c>, otherwise returns result of running <paramref name="onNone"/>
    /// </summary>
    /// <param name="onSome">The function to run if <paramref name="input"/> is <c>Some</c></param>
    /// <param name="onNone">The function to run if <paramref name="input"/> is <c>None</c></param>
    /// <param name="input">The input option.</param>
    /// <returns>
    /// The result of running <paramref name="onSome"/> if the input is <c>Some</c>, else returns result of running <paramref name="onNone"/>.
    /// </returns>
    let inline either
        ([<InlineIfLambda>] onSome: 'input -> Job<'output>)
        (onNone: Job<'output>)
        (input: Job<'input option>)
        : Job<'output> =
        input
        |> Job.bind (
            function
            | Some v -> onSome v
            | None -> onNone
        )
