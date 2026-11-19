namespace FsToolkit.ErrorHandling

open System.Threading.Tasks
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end

[<RequireQualifiedAccess>]
module TaskResultOption =
    let inline map ([<InlineIfLambda>] f) tro = TaskResult.map (Option.map f) tro

    let inline bind ([<InlineIfLambda>] f) tro =
        LogRiteLogging._logger.LogError(">>> Entering bind()")
        let binder opt =
            LogRiteLogging._logger.LogError(">>> Entering binder(opt={Opt})", opt)
            LogRiteLogging._logger.LogWarning("<<< Exiting binder(opt={Opt})", opt)
            match opt with
            | Some x -> f x
            | None -> TaskResult.ok None

        TaskResult.bind binder tro

    let inline map2 ([<InlineIfLambda>] f) xTRO yTRO =
        LogRiteLogging._logger.LogError(">>> Entering map2()")
        LogRiteLogging._logger.LogWarning("<<< Exiting map2()")
        TaskResult.map2 (Option.map2 f) xTRO yTRO

    let inline map3 ([<InlineIfLambda>] f) xTRO yTRO zTRO =
        LogRiteLogging._logger.LogError(">>> Entering map3()")
        LogRiteLogging._logger.LogWarning("<<< Exiting map3()")
        TaskResult.map3 (Option.map3 f) xTRO yTRO zTRO

    let inline singleton value = TaskResult.ok (Some value)

    let inline ok x = singleton x

    let inline error x : TaskResult<'ok option, 'error> = TaskResult.error x

    let inline apply fTRO xTRO = map2 (fun f x -> f x) fTRO xTRO

    /// Replaces the wrapped value with unit
    let inline ignore<'ok, 'error> (tro: Task<Result<'ok option, 'error>>) =
        LogRiteLogging._logger.LogError(">>> Entering ignore(tro={Tro})", tro)
        LogRiteLogging._logger.LogWarning("<<< Exiting ignore(tro={Tro})", tro)
        tro
        |> map ignore<'ok>

    /// <summary>
    /// Transforms a <c>Result&lt;'ok, 'error&gt;</c> into a <c>Task&lt;Result&lt;'ok option, 'error&gt;&gt;</c>.
    /// </summary>
    let inline ofResult (result: Result<'ok, 'error>) : Task<Result<'ok option, 'error>> =
        LogRiteLogging._logger.LogError(">>> Entering ofResult(result={Result})", result)
        LogRiteLogging._logger.LogWarning("<<< Exiting ofResult(result={Result})", result)
        result
        |> Result.map Some
        |> Task.singleton

    /// <summary>
    /// Transforms a <c>Task&lt;Result&lt;'ok, 'error&gt;&gt;</c> into a <c>Task&lt;Result&lt;'ok option, 'error&gt;&gt;</c>.
    /// </summary>
    let inline ofTaskResult
        (taskResult: Task<Result<'ok, 'error>>)
        : Task<Result<'ok option, 'error>> =
        taskResult
        |> TaskResult.map Some

    /// <summary>
    /// Transforms a <c>'ok option</c> into a <c>Task&lt;Result&lt;'ok option, 'error&gt;&gt;</c>.
    /// </summary>
    let inline ofOption (option: 'ok option) : Task<Result<'ok option, 'error>> =
        option
        |> Ok
        |> Task.singleton

    /// <summary>
    /// Transforms a <c>Task&lt;'ok option&gt;</c> into a <c>Task&lt;Result&lt;'ok option, 'error&gt;&gt;</c>.
    /// </summary>
    let inline ofTaskOption (taskOption: Task<'ok option>) : Task<Result<'ok option, 'error>> =
        taskOption
        |> Task.map Ok

    /// <summary>
    /// Transforms a <c>Task&lt;'ok&gt;</c> into a <c>Task&lt;Result&lt;'ok option, 'error&gt;&gt;</c>.
    /// </summary>
    let inline ofTask (task: Task<'ok>) : Task<Result<'ok option, 'error>> =
        task
        |> Task.map (Some >> Ok)
