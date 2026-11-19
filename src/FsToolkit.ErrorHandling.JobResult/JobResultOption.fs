namespace FsToolkit.ErrorHandling
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module JobResultOption =
    open Hopac
    let inline map ([<InlineIfLambda>] f) jro = JobResult.map (Option.map f) jro

    let inline bind ([<InlineIfLambda>] f) jro =
        LogRiteLogging._logger.LogError(">>> Entering bind()")
        let binder opt =
            LogRiteLogging._logger.LogError(">>> Entering binder(opt={Opt})", opt)
            LogRiteLogging._logger.LogWarning("<<< Exiting binder(opt={Opt})", opt)
            match opt with
            | Some x -> f x
            | None -> JobResult.singleton None

        JobResult.bind binder jro

    let inline map2 ([<InlineIfLambda>] f) xJRO yJRO =
        LogRiteLogging._logger.LogError(">>> Entering map2()")
        JobResult.map2 (Option.map2 f) xJRO yJRO

    let inline map3 ([<InlineIfLambda>] f) xJRO yJRO zJRO =
        LogRiteLogging._logger.LogError(">>> Entering map3()")
        JobResult.map3 (Option.map3 f) xJRO yJRO zJRO

    let inline singleton value =
        LogRiteLogging._logger.LogError(">>> Entering singleton(value={Value})", value)
        LogRiteLogging._logger.LogWarning("<<< Exiting singleton(value={Value})", value)
        Some value
        |> Ok
        |> Job.result

    let apply fJRO xJRO = map2 (fun f x -> f x) fJRO xJRO

    /// Replaces the wrapped value with unit
    let inline ignore<'a, 'b> (jro: Job<Result<'a option, 'b>>) =
        LogRiteLogging._logger.LogError(">>> Entering ignore(jro={Jro})", jro)
        LogRiteLogging._logger.LogWarning("<<< Exiting ignore(jro={Jro})", jro)
        jro
        |> map ignore<'a>
