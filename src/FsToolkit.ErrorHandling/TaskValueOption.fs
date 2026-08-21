namespace FsToolkit.ErrorHandling

open System.Threading.Tasks
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end

[<RequireQualifiedAccess>]
module TaskValueOption =

    let inline map ([<InlineIfLambda>] f) ar = Task.map (ValueOption.map f) ar

    let inline bind ([<InlineIfLambda>] f) (ar: Task<_>) =
        task {
        LogRiteLogging._logger.LogDebug(">>> Entering inline(bind={Bind},ar={Ar})", bind, ar)
            let! opt = ar
            LogRiteLogging._logger.LogDebug("<<< Exiting inline(bind={Bind},ar={Ar})", bind, ar)

            let t =
                LogRiteLogging._logger.LogDebug(">>> Entering t()")
                match opt with
                    LogRiteLogging._logger.LogDebug("<<< Exiting t()")
                | ValueSome x -> f x
                | ValueNone -> Task.singleton ValueNone

            return! t
        }

    let inline valueSome x = Task.singleton (ValueSome x)

    let inline apply f x =
        LogRiteLogging._logger.LogDebug(">>> Entering inline(apply={Apply},f={F},x={X})", apply, f, x)
        bind (fun f' -> bind (fun x' -> valueSome (f' x')) x) f

    let inline zip x1 x2 =
        Task.zip x1 x2
        LogRiteLogging._logger.LogDebug(">>> Entering inline(zip={Zip},x1={X1},x2={X2})", zip, x1, x2)
        |> Task.map (fun (r1, r2) -> ValueOption.zip r1 r2)


    /// <summary>
    /// Returns result of running <paramref name="onValueSome"/> if it is <c>ValueSome</c>, otherwise returns result of running <paramref name="onValueNone"/>
    /// </summary>
    /// <param name="onValueSome">The function to run if <paramref name="input"/> is <c>ValueSome</c></param>
    /// <param name="onValueNone">The function to run if <paramref name="input"/> is <c>ValueNone</c></param>
    /// <param name="input">The input voption.</param>
    /// <returns>
    /// The result of running <paramref name="onValueSome"/> if the input is <c>ValueSome</c>, else returns result of running <paramref name="onValueNone"/>.
    /// </returns>
    let inline either
        ([<InlineIfLambda>] onValueSome: 'input -> Task<'output>)
        ([<InlineIfLambda>] onValueNone: unit -> Task<'output>)
        (input: Task<'input voption>)
        : Task<'output> =
        input
        |> Task.bind (
            function
            | ValueSome v -> onValueSome v
            | ValueNone -> onValueNone ()
        )

    /// <summary>
    ///  Gets the value of the option if the option is <c>Some</c>, otherwise returns the specified default value.
    /// </summary>
    /// <param name="value">The specified default value.</param>
    /// <param name="taskValueOption">The input option.</param>
    /// <returns>
    /// The option if the option is <c>Some</c>, else the default value.
    /// </returns>
    let inline defaultValue (value: 'value) (taskValueOption: Task<'value voption>) =
        taskValueOption
        |> Task.map (ValueOption.defaultValue value)

    /// <summary>
    ///  Gets the value of the voption if the voption is <c>ValueSome</c>, otherwise evaluates <paramref name="defThunk"/> and returns the result.
    /// </summary>
    /// <param name="defThunk">A thunk that provides a default value when evaluated.</param>
    /// <param name="taskValueOption">The input voption.</param>
    /// <returns>
    /// The voption if the option is <c>ValueSome</c>, else the result of evaluating <paramref name="defThunk"/>.
    /// </returns>
    let inline defaultWith
        ([<InlineIfLambda>] defThunk: unit -> 'value)
        (taskValueOption: Task<'value voption>)
        : Task<'value> =
        taskValueOption
        |> Task.map (ValueOption.defaultWith defThunk)
