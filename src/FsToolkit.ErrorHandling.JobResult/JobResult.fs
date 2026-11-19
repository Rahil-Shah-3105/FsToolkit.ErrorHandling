namespace FsToolkit.ErrorHandling

open Hopac
open Hopac.Infixes
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module JobResult =

    let inline map ([<InlineIfLambda>] f) jr = Job.map (Result.map f) jr

    let inline mapError ([<InlineIfLambda>] f) jr = Job.map (Result.mapError f) jr

    let inline bind ([<InlineIfLambda>] f: 'a -> Job<Result<'c, 'b>>) (jr: Job<Result<'a, 'b>>) =
        LogRiteLogging._logger.LogError(">>> Entering bind()")
        LogRiteLogging._logger.LogWarning("<<< Exiting bind()")
        Job.bind
            (Result.either
                f
                (Error
                 >> Job.result))
            jr

    let inline eitherMap ([<InlineIfLambda>] onSuccess) ([<InlineIfLambda>] onError) jr =
        LogRiteLogging._logger.LogError(">>> Entering eitherMap()")
        LogRiteLogging._logger.LogWarning("<<< Exiting eitherMap()")
        Job.map (Result.eitherMap onSuccess onError) jr

    let inline ofAsync aAsync =
        LogRiteLogging._logger.LogError(">>> Entering ofAsync(aAsync={AAsync})", aAsync)
        LogRiteLogging._logger.LogWarning("<<< Exiting ofAsync(aAsync={AAsync})", aAsync)
        aAsync
        |> Job.fromAsync
        |> Job.catch
        |> Job.map Result.ofChoice

    let inline fromTask aTask =
        LogRiteLogging._logger.LogError(">>> Entering fromTask(aTask={ATask})", aTask)
        LogRiteLogging._logger.LogWarning("<<< Exiting fromTask(aTask={ATask})", aTask)
        aTask
        |> Job.fromTask
        |> Job.catch
        |> Job.map Result.ofChoice

    let inline fromUnitTask aTask =
        LogRiteLogging._logger.LogError(">>> Entering fromUnitTask(aTask={ATask})", aTask)
        LogRiteLogging._logger.LogWarning("<<< Exiting fromUnitTask(aTask={ATask})", aTask)
        aTask
        |> Job.fromUnitTask
        |> Job.catch
        |> Job.map Result.ofChoice

    let inline singleton x =
        LogRiteLogging._logger.LogError(">>> Entering singleton(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting singleton(x={X})", x)
        Ok x
        |> Job.result

    let inline error x =
        LogRiteLogging._logger.LogError(">>> Entering error(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting error(x={X})", x)
        Error x
        |> Job.result

    let inline map2 ([<InlineIfLambda>] f) xJR yJR = Job.map2 (Result.map2 f) xJR yJR

    let inline map3 ([<InlineIfLambda>] f) xJR yJR zJR = Job.map3 (Result.map3 f) xJR yJR zJR

    let inline apply fJR xJR = map2 (fun f x -> f x) fJR xJR


    /// <summary>
    /// Returns <paramref name="result"/> if it is <c>Ok</c>, otherwise returns <paramref name="ifError"/>
    /// </summary>
    /// <param name="ifError">The value to use if <paramref name="result"/> is <c>Error</c></param>
    /// <param name="result">The input result.</param>
    /// <remarks>
    /// </remarks>
    /// <example>
    /// <code>
    ///     JobResult.error "First" |> JobResult.orElse (JobResult.error "Second") // evaluates to Error ("Second")
    ///     JobResult.error "First" |> JobResult.orElse (JobResult.singleton "Second") // evaluates to Ok ("Second")
    ///     JobResult.singleton "First" |> JobResult.orElse (JobResult.error "Second") // evaluates to Ok ("First")
    ///     JobResult.singleton "First" |> JobResult.orElse (JobResult.singleton "Second") // evaluates to Ok ("First")
    /// </code>
    /// </example>
    /// <returns>
    /// The result if the result is Ok, else returns <paramref name="ifError"/>.
    /// </returns>
    let inline orElse (ifError: Job<Result<'ok, 'error2>>) (result: Job<Result<'ok, 'error>>) =
        LogRiteLogging._logger.LogError(">>> Entering orElse(ifError={IfError})", ifError)
        LogRiteLogging._logger.LogWarning("<<< Exiting orElse(ifError={IfError})", ifError)
        result
        |> Job.bind (Result.either singleton (fun _ -> ifError))

    /// <summary>
    /// Returns <paramref name="result"/> if it is <c>Ok</c>, otherwise executes <paramref name="ifErrorFunc"/> and returns the result.
    /// </summary>
    /// <param name="ifErrorFunc">A function that provides an alternate result when evaluated.</param>
    /// <param name="result">The input result.</param>
    /// <remarks>
    /// <paramref name="ifErrorFunc"/>  is not executed unless <paramref name="result"/> is an <c>Error</c>.
    /// </remarks>
    /// <example>
    /// <code>
    ///     JobResult.error "First" |> JobResult.orElseWith (fun _ -> JobResult.error "Second") // evaluates to Error ("Second")
    ///     JobResult.error "First" |> JobResult.orElseWith (fun _ -> JobResult.singleton "Second") // evaluates to Ok ("Second")
    ///     JobResult.singleton "First" |> JobResult.orElseWith (fun _ -> JobResult.error "Second") // evaluates to Ok ("First")
    ///     JobResult.singleton "First" |> JobResult.orElseWith (fun _ -> JobResult.singleton "Second") // evaluates to Ok ("First")
    /// </code>
    /// </example>
    /// <returns>
    /// The result if the result is Ok, else the result of executing <paramref name="ifErrorFunc"/>.
    /// </returns>
    let inline orElseWith
        ([<InlineIfLambda>] ifErrorFunc: 'error -> Job<Result<'ok, 'error2>>)
        (result: Job<Result<'ok, 'error>>)
        =
        result
        |> Job.bind (Result.either singleton ifErrorFunc)

    /// Replaces the wrapped value with unit
    let inline ignore<'ok, 'error> (jr: Job<Result<'ok, 'error>>) =
        LogRiteLogging._logger.LogError(">>> Entering ignore(jr={Jr})", jr)
        LogRiteLogging._logger.LogWarning("<<< Exiting ignore(jr={Jr})", jr)
        jr
        |> map ignore<'ok>

    /// Returns the specified error if the job-wrapped value is false.
    let inline requireTrue error value =
        LogRiteLogging._logger.LogError(">>> Entering requireTrue(error={Error},value={Value})", error, value)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireTrue(error={Error},value={Value})", error, value)
        value
        |> Job.map (Result.requireTrue error)

    /// Returns the specified error if the job-wrapped value is true.
    let inline requireFalse error value =
        LogRiteLogging._logger.LogError(">>> Entering requireFalse(error={Error},value={Value})", error, value)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireFalse(error={Error},value={Value})", error, value)
        value
        |> Job.map (Result.requireFalse error)

    // Converts an job-wrapped Option to a Result, using the given error if None.
    let inline requireSome error option =
        LogRiteLogging._logger.LogError(">>> Entering requireSome(error={Error},option={Option})", error, option)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireSome(error={Error},option={Option})", error, option)
        option
        |> Job.map (Result.requireSome error)

    // Converts an job-wrapped Option to a Result, using the given error factory if None.
    let inline requireSomeWith ([<InlineIfLambda>] errorFactory: unit -> 'error) option =
        LogRiteLogging._logger.LogError(">>> Entering requireSomeWith()")
        LogRiteLogging._logger.LogWarning("<<< Exiting requireSomeWith()")
        option
        |> Job.map (Result.requireSomeWith errorFactory)

    // Converts an job-wrapped Option to a Result, using the given error if Some.
    let inline requireNone error option =
        LogRiteLogging._logger.LogError(">>> Entering requireNone(error={Error},option={Option})", error, option)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireNone(error={Error},option={Option})", error, option)
        option
        |> Job.map (Result.requireNone error)

    // Converts an job-wrapped Option to a Result, using the given error factory if Some.
    let inline requireNoneWith ([<InlineIfLambda>] errorFactory: unit -> 'error) option =
        LogRiteLogging._logger.LogError(">>> Entering requireNoneWith()")
        LogRiteLogging._logger.LogWarning("<<< Exiting requireNoneWith()")
        option
        |> Job.map (Result.requireNoneWith errorFactory)

    // Converts an job-wrapped ValueOption to a Result, using the given error if ValueNone.
    let inline requireValueSome error voption =
        LogRiteLogging._logger.LogError(">>> Entering requireValueSome(error={Error},voption={Voption})", error, voption)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireValueSome(error={Error},voption={Voption})", error, voption)
        voption
        |> Job.map (Result.requireValueSome error)

    // Converts an job-wrapped ValueOption to a Result, using the given error if ValueSome.
    let inline requireValueNone error voption =
        LogRiteLogging._logger.LogError(">>> Entering requireValueNone(error={Error},voption={Voption})", error, voption)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireValueNone(error={Error},voption={Voption})", error, voption)
        voption
        |> Job.map (Result.requireValueNone error)

    /// Returns Ok if the job-wrapped value and the provided value are equal, or the specified error if not.
    let inline requireEqual x1 x2 error =
        LogRiteLogging._logger.LogError(">>> Entering requireEqual(x1={X1},x2={X2},error={Error})", x1, x2, error)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireEqual(x1={X1},x2={X2},error={Error})", x1, x2, error)
        x2
        |> Job.map (fun x2' -> Result.requireEqual x1 x2' error)

    /// Returns Ok if the two values are equal, or the specified error if not.
    let inline requireEqualTo other error this =
        LogRiteLogging._logger.LogError(">>> Entering requireEqualTo(other={Other},error={Error},this={This})", other, error, this)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireEqualTo(other={Other},error={Error},this={This})", other, error, this)
        this
        |> Job.map (Result.requireEqualTo other error)

    /// Returns Ok if the job-wrapped sequence is empty, or the specified error if not.
    let inline requireEmpty error xs =
        LogRiteLogging._logger.LogError(">>> Entering requireEmpty(error={Error},xs={Xs})", error, xs)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireEmpty(error={Error},xs={Xs})", error, xs)
        xs
        |> Job.map (Result.requireEmpty error)

    /// Returns Ok if the job-wrapped sequence is not-empty, or the specified error if not.
    let inline requireNotEmpty error xs =
        LogRiteLogging._logger.LogError(">>> Entering requireNotEmpty(error={Error},xs={Xs})", error, xs)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireNotEmpty(error={Error},xs={Xs})", error, xs)
        xs
        |> Job.map (Result.requireNotEmpty error)

    /// Returns the first item of the job-wrapped sequence if it exists, or the specified
    /// error if the sequence is empty
    let inline requireHead error xs =
        LogRiteLogging._logger.LogError(">>> Entering requireHead(error={Error},xs={Xs})", error, xs)
        LogRiteLogging._logger.LogWarning("<<< Exiting requireHead(error={Error},xs={Xs})", error, xs)
        xs
        |> Job.map (Result.requireHead error)

    /// Replaces an error value of an job-wrapped result with a custom error
    /// value.
    let inline setError error jobResult =
        LogRiteLogging._logger.LogError(">>> Entering setError(error={Error},jobResult={JobResult})", error, jobResult)
        LogRiteLogging._logger.LogWarning("<<< Exiting setError(error={Error},jobResult={JobResult})", error, jobResult)
        jobResult
        |> Job.map (Result.setError error)

    /// Replaces a unit error value of an job-wrapped result with a custom
    /// error value. Safer than setError since you're not losing any information.
    let inline withError error jobResult =
        LogRiteLogging._logger.LogError(">>> Entering withError(error={Error},jobResult={JobResult})", error, jobResult)
        LogRiteLogging._logger.LogWarning("<<< Exiting withError(error={Error},jobResult={JobResult})", error, jobResult)
        jobResult
        |> Job.map (Result.withError error)

    /// Extracts the contained value of an job-wrapped result if Ok, otherwise
    /// uses ifError.
    let inline defaultValue ifError jobResult =
        LogRiteLogging._logger.LogError(">>> Entering defaultValue(ifError={IfError},jobResult={JobResult})", ifError, jobResult)
        LogRiteLogging._logger.LogWarning("<<< Exiting defaultValue(ifError={IfError},jobResult={JobResult})", ifError, jobResult)
        jobResult
        |> Job.map (Result.defaultValue ifError)

    /// Extracts the contained value of an job-wrapped result if Error, otherwise
    /// uses ifOk.
    let inline defaultError ifOk jobResult =
        LogRiteLogging._logger.LogError(">>> Entering defaultError(ifOk={IfOk},jobResult={JobResult})", ifOk, jobResult)
        LogRiteLogging._logger.LogWarning("<<< Exiting defaultError(ifOk={IfOk},jobResult={JobResult})", ifOk, jobResult)
        jobResult
        |> Job.map (Result.defaultError ifOk)

    /// Extracts the contained value of an job-wrapped result if Ok, otherwise
    /// evaluates ifErrorThunk and uses the result.
    let inline defaultWith ([<InlineIfLambda>] ifErrorThunk: 'error -> 'ok) jobResult =
        LogRiteLogging._logger.LogError(">>> Entering defaultWith()")
        LogRiteLogging._logger.LogWarning("<<< Exiting defaultWith()")
        jobResult
        |> Job.map (Result.defaultWith ifErrorThunk)

    /// Same as defaultValue for a result where the Ok value is unit. The name
    /// describes better what is actually happening in this case.
    let inline ignoreError<'error> (jobResult: Job<Result<unit, 'error>>) =
        LogRiteLogging._logger.LogError(">>> Entering ignoreError(jobResult={JobResult})", jobResult)
        LogRiteLogging._logger.LogWarning("<<< Exiting ignoreError(jobResult={JobResult})", jobResult)
        defaultValue () jobResult

    /// If the job-wrapped result is Ok, executes the function on the Ok value.
    /// Passes through the input value.
    let inline tee ([<InlineIfLambda>] f) jobResult =
        LogRiteLogging._logger.LogError(">>> Entering tee()")
        LogRiteLogging._logger.LogWarning("<<< Exiting tee()")
        jobResult
        |> Job.map (Result.tee f)

    /// If the job-wrapped result is Ok and the predicate returns true, executes
    /// the function on the Ok value. Passes through the input value.
    let inline teeIf ([<InlineIfLambda>] predicate) ([<InlineIfLambda>] f) jobResult =
        LogRiteLogging._logger.LogError(">>> Entering teeIf()")
        LogRiteLogging._logger.LogWarning("<<< Exiting teeIf()")
        jobResult
        |> Job.map (Result.teeIf predicate f)

    /// If the job-wrapped result is Error, executes the function on the Error
    /// value. Passes through the input value.
    let inline teeError ([<InlineIfLambda>] f) jobResult =
        LogRiteLogging._logger.LogError(">>> Entering teeError()")
        LogRiteLogging._logger.LogWarning("<<< Exiting teeError()")
        jobResult
        |> Job.map (Result.teeError f)

    /// If the job-wrapped result is Error and the predicate returns true,
    /// executes the function on the Error value. Passes through the input value.
    let inline teeErrorIf ([<InlineIfLambda>] predicate) ([<InlineIfLambda>] f) jobResult =
        LogRiteLogging._logger.LogError(">>> Entering teeErrorIf()")
        LogRiteLogging._logger.LogWarning("<<< Exiting teeErrorIf()")
        jobResult
        |> Job.map (Result.teeErrorIf predicate f)

    /// Takes two results and returns a tuple of the pair
    let inline zip j1 j2 =
        LogRiteLogging._logger.LogError(">>> Entering zip(j1={J1},j2={J2})", j1, j2)
        LogRiteLogging._logger.LogWarning("<<< Exiting zip(j1={J1},j2={J2})", j1, j2)
        Job.zip j1 j2
        |> Job.map (fun (r1, r2) -> Result.zip r1 r2)

    /// Takes two results and returns a tuple of the error pair
    let inline zipError j1 j2 =
        LogRiteLogging._logger.LogError(">>> Entering zipError(j1={J1},j2={J2})", j1, j2)
        LogRiteLogging._logger.LogWarning("<<< Exiting zipError(j1={J1},j2={J2})", j1, j2)
        Job.zip j1 j2
        |> Job.map (fun (r1, r2) -> Result.zipError r1 r2)

    /// Catches exceptions and maps them to the Error case using the provided function.
    let inline catch f x =
        LogRiteLogging._logger.LogError(">>> Entering catch(f={F},x={X})", f, x)
        LogRiteLogging._logger.LogWarning("<<< Exiting catch(f={F},x={X})", f, x)
        x
        |> Job.catch
        |> Job.map (
            function
            | Choice1Of2(Ok v) -> Ok v
            | Choice1Of2(Error err) -> Error err
            | Choice2Of2 ex -> Error(f ex)
        )

    /// Lift Job to JobResult
    let inline ofJob x =
        LogRiteLogging._logger.LogError(">>> Entering ofJob(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting ofJob(x={X})", x)
        x
        |> Job.map Ok

    /// Lift Result to JobResult
    let inline ofResult (x: Result<_, _>) =
        LogRiteLogging._logger.LogError(">>> Entering ofResult(x={X})", x)
        LogRiteLogging._logger.LogWarning("<<< Exiting ofResult(x={X})", x)
        x
        |> Job.singleton

    /// Bind the JobResult and requireSome on the inner option value.
    let inline bindRequireSome error x =
        LogRiteLogging._logger.LogError(">>> Entering bindRequireSome(error={Error},x={X})", error, x)
        LogRiteLogging._logger.LogWarning("<<< Exiting bindRequireSome(error={Error},x={X})", error, x)
        x
        |> bind (
            Result.requireSome error
            >> Job.singleton
        )

    /// Bind the JobResult and requireNone on the inner option value.
    let inline bindRequireNone error x =
        LogRiteLogging._logger.LogError(">>> Entering bindRequireNone(error={Error},x={X})", error, x)
        LogRiteLogging._logger.LogWarning("<<< Exiting bindRequireNone(error={Error},x={X})", error, x)
        x
        |> bind (
            Result.requireNone error
            >> Job.singleton
        )

    /// Bind the JobResult and requireValueSome on the inner voption value.
    let inline bindRequireValueSome error x =
        LogRiteLogging._logger.LogError(">>> Entering bindRequireValueSome(error={Error},x={X})", error, x)
        LogRiteLogging._logger.LogWarning("<<< Exiting bindRequireValueSome(error={Error},x={X})", error, x)
        x
        |> bind (
            Result.requireValueSome error
            >> Job.singleton
        )

    /// Bind the JobResult and requireValueNone on the inner voption value.
    let inline bindRequireValueNone error x =
        LogRiteLogging._logger.LogError(">>> Entering bindRequireValueNone(error={Error},x={X})", error, x)
        LogRiteLogging._logger.LogWarning("<<< Exiting bindRequireValueNone(error={Error},x={X})", error, x)
        x
        |> bind (
            Result.requireValueNone error
            >> Job.singleton
        )
