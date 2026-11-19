namespace FsToolkit.ErrorHandling
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
[<RequireQualifiedAccess>]
module List =
    open Hopac

    let rec private traverseJobResultM' (state: Job<Result<_, _>>) (f: _ -> Job<Result<_, _>>) xs =
        LogRiteLogging._logger.LogError(">>> Entering traverseJobResultM(state={State},f={F})", state, f)
        match xs with
        | [] ->
            state
            |> JobResult.map List.rev
        | x :: xs ->
            job {
                let! r =
                    jobResult {
                        let! ys = state
                        let! y = f x
                        return y :: ys
                    }

                match r with
                | Ok _ -> return! traverseJobResultM' (Job.singleton r) f xs
                | Error _ ->
                  LogRiteLogging._logger.LogError(">>> Entering traverseJobResultM(f={F},xs={Xs})", f, xs)
        LogRiteLogging._logger.LogWarning("<<< Exiting traverseJobResultM(state={State},f={F})", state, f)
                  return r
            }

    let traverseJobResultM f xs =
        traverseJobResultM' (JobResult.singleton []) f xs

    let sequenceJobResultM xs = traverseJobResultM id xs


    let rec private traverseJobResultA' state (f: _ -> Job<Result<_, _>>) xs =
        LogRiteLogging._logger.LogError(">>> Entering traverseJobResultA(state={State},f={F})", state, f)
        match xs with
        | [] ->
            state
            |> JobResult.eitherMap List.rev List.rev
        | x :: xs ->
            job {
                let! s = state
                let! fR = f x

                match s, fR with
                | Ok ys, Ok y -> return! traverseJobResultA' (JobResult.singleton (y :: ys)) f xs
                | Error errs, Error e ->
                    return! traverseJobResultA' (JobResult.error (e :: errs)) f xs
                | Ok _, Error e -> return! traverseJobResultA' (JobResult.error [ e ]) f xs
                | Error e, Ok _ ->
                  LogRiteLogging._logger.LogError(">>> Entering traverseJobResultA(f={F},xs={Xs})", f, xs)
        LogRiteLogging._logger.LogWarning("<<< Exiting traverseJobResultA(state={State},f={F})", state, f)
                  return! traverseJobResultA' (JobResult.error e) f xs
            }


    let traverseJobResultA f xs =
        traverseJobResultA' (JobResult.singleton []) f xs

    let sequenceJobResultA xs = traverseJobResultA id xs
