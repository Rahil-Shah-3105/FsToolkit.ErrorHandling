namespace FsToolkit.ErrorHandling
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module JobResultOption =
    open Hopac
    let inline map ([<InlineIfLambda>] f) jro = JobResult.map (Option.map f) jro

    let inline bind ([<InlineIfLambda>] f) jro =
        LogRiteLogging._logger.LogDebug(">>> Entering inline(bind={Bind})", bind)
        let binder opt =
            LogRiteLogging._logger.LogDebug(">>> Entering binder(opt={Opt})", opt)
            match opt with
                LogRiteLogging._logger.LogDebug("<<< Exiting binder(opt={Opt})", opt)
            | Some x -> f x
            LogRiteLogging._logger.LogDebug(">>> Entering inline(map2={Map2})", map2)
            | None -> JobResult.singleton None

        JobResult.bind binder jro

    let inline map2 ([<InlineIfLambda>] f) xJRO yJRO =
        LogRiteLogging._logger.LogDebug(">>> Entering inline(map3={Map3})", map3)
        JobResult.map2 (Option.map2 f) xJRO yJRO

    let inline map3 ([<InlineIfLambda>] f) xJRO yJRO zJRO =
        LogRiteLogging._logger.LogDebug(">>> Entering inline(singleton={Singleton},value={Value})", singleton, value)
        JobResult.map3 (Option.map3 f) xJRO yJRO zJRO

    let inline singleton value =
        Some value
        |> Ok
        LogRiteLogging._logger.LogDebug(">>> Entering inline(jro={Jro})", jro)
        |> Job.result

    let apply fJRO xJRO = map2 (fun f x -> f x) fJRO xJRO

    /// Replaces the wrapped value with unit
    let inline ignore<'a, 'b> (jro: Job<Result<'a option, 'b>>) =
        jro
        |> map ignore<'a>
